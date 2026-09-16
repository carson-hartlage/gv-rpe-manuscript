#' make geocode history
#'
#' Read from Address_History raw data to:
#'
#' - convert to addr vector by cleaning and parsing addresses
#' - impute start dates using historical midpoint approach
#' - geocode non-missing, unique cleaned addresses
#' - add s2 cell and 2020 census block group id
#' @param n_days_before_first number of days to stretch address history before
#' first known address
#' @details The geocoding with DeGAUSS step is memoised so that if the
#' same unique set of addresses are used, cached results will be loaded
#' instead of calling docker for DeGAUSS
#' @return tibble of geocoded address details by `MRN` and imputed start date
#' @export
#' @examples
#' \dontrun{
#' make_geocode_history() |>
#'  dplyr::select(MRN, imputed_start_date, s2_cell, tiger_bg_2020)
#' }
set.seed(1234)
make_geocode_history <- function(n_days_before_first = 30, n_days_after_last = 30) {
  cli::cli_progress_step("reading Address_History")
  rd <-
    readr::read_csv(
      "data-raw/Address_History.csv",
      col_types = readr::cols_only(
        MRN = readr::col_character(),
        ADD_LINE1 = readr::col_character(),
        ADD_LINE2 = readr::col_character(),
        CITY = readr::col_character(),
        STATE = readr::col_character(),
        ZIP = readr::col_character(),
        EFF_START_DATE = readr::col_date(format = "%m/%d/%Y")
      )
    )
  # convert to addr vector and use cleaned/parsed address for geocoding
  as_addr_memoised <- memoise::memoise(
    addr::as_addr,
    cache = memoise::cache_filesystem("as_addr_batch_cache")
  )
  cli::cli_progress_step("parsing and cleaning addrs")
  d_addr <- rd |>
    tidyr::unite(
      "address",
      c(ADD_LINE1, ADD_LINE2, CITY, STATE, ZIP),
      sep = " ",
      na.rm = TRUE
    ) |>
    dplyr::mutate(addr = as_addr_memoised(address)) |>
    dplyr::mutate(address_for_geocoding = as.character(addr))
  
  # impute start dates using historical midpoint approach
  cli::cli_progress_step("imputing start dates with historical midpoints")
  d_addr <-
    d_addr |>
    group_by(MRN, EFF_START_DATE) |>
    slice_sample(n = 1) |>
    ungroup() %>%
    dplyr::group_by(MRN) |>
    dplyr::arrange(EFF_START_DATE, .by_group = TRUE) |>
    dplyr::mutate(
      imputed_start_date = impute_start_dates(
        EFF_START_DATE,
        start_early = n_days_before_first
      )
    ) |>
    dplyr::ungroup()
  
  # impute end dates
  cli::cli_progress_step("imputing end dates")
  d_addr <-
    d_addr |>
    dplyr::group_by(MRN) |>
    dplyr::arrange(imputed_start_date, .by_group = TRUE) |>
    dplyr::mutate(
      imputed_end_date = dplyr::coalesce(
        dplyr::lead(imputed_start_date),
        imputed_start_date + n_days_after_last
      )
    ) |>
    dplyr::ungroup()
  
  # ## calculate end date based on next start date (and other encounter dates??)
  # d_addr <-
  #   d_addr |>
  #   group_by(MRN) |>
  #   arrange(EFF_START_DATE, .by_group = TRUE) |>
  #   mutate(
  #     imputed_end_date = coalesce(
  #       lead(imputed_start_date),
  #       EFF_START_DATE + n_days_after_last
  #     )
  #   ) |>
  #   ungroup()
  
  ## geocode non-missing, unique cleaned addresses
  cli::cli_progress_step("geocoding non-missing, unique, cleaned addresses")
  d_for_geocode <-
    d_addr |>
    dplyr::filter(!is.na(addr)) |>
    dplyr::select(address = address_for_geocoding) |>
    unique() |>
    dplyr::pull(address) |>
    sort()
  degauss_geocode_memoised <- memoise::memoise(
    degauss_geocode,
    cache = memoise::cache_filesystem("degauss_batch_geocode_cache")
  )
  d_geocode <- degauss_geocode_memoised(d_for_geocode)
  
  d_out <- dplyr::left_join(
    d_addr,
    d_geocode,
    by = c("address_for_geocoding" = "address")
  )
  
  cli::cli_progress_step("adding s2 cell")
  d_out$s2_cell <- with(d_out, s2::as_s2_cell(s2::s2_lnglat(lon, lat)))
  cli::cli_progress_step("adding 2020 census block group id")
  d_out$tiger_bg_2020 <- with(
    d_out,
    addr::s2_join_tiger_bg(s2_cell, year = "2020")
  )
  
  cli::cli_progress_done()
  return(d_out)
}

utils::globalVariables(c(
  "ADD_LINE1",
  "ADD_LINE2",
  "CITY",
  "STATE",
  "ZIP",
  "address",
  "addr",
  "MRN",
  "EFF_START_DATE",
  "address_for_geocoding"
))

a <- make_geocode_history()
saveRDS(a, "~/Desktop/PhD/Aim2/address_history_2.23.25.rds")
