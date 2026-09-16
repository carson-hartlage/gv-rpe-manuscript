library(tidyverse)
library(sf)
library(lubridate)
library(readr)
setwd("/Users/carsonhartlage/Documents/GitHub/crime-pe-causal-inference/crime")
source("street_range_functions.R")

# read in data 
d <- read_csv("https://data.cincinnati-oh.gov/api/views/k59e-2pvf/rows.csv?accessType=DOWNLOAD",
              col_types = cols_only(
                INSTANCEID = col_character(),
                INCIDENT_NO = col_character(),
                DATE_FROM = col_datetime(format = "%m/%d/%Y %I:%M:%S %p"),
                OFFENSE = col_character(),
                ADDRESS_X = col_character()
              ))

# clean up and match to coarse crime categories
crime_category <- yaml::read_yaml("crime_categories.yaml")
  
crime_category <- 
  tibble::tibble(category = unlist(purrr::map(crime_category, names)), 
                 OFFENSE = purrr::map(crime_category$category, ~.x[[1]])) |>
  unnest(cols = OFFENSE)

d <- 
  d |> 
  filter(DATE_FROM >= as.Date("2015-08-31")) |>
  filter(DATE_FROM <= as.Date("2025-03-31")) |> # filter by crime start date
  left_join(crime_category, by = "OFFENSE") |>  # assign offense crime_category
  distinct(.keep_all = TRUE) |> # remove duplicated rows
  filter(!is.na(ADDRESS_X))  # remove missing address

# count crimes by category
d_crime_by_street_range <- 
  d |> 
  mutate(n = 1) |>
  pivot_wider(names_from = category, 
              values_from = n) |> 
  mutate(across(c(property, violent, other), ~replace_na(.x, 0))) |>
  select(c(violent, property, other, DATE_FROM, ADDRESS_X))

# make street geometries then join back to crime incidents
city_streets <- data.frame(ADDRESS_X = unique(d_crime_by_street_range$ADDRESS_X))

# transform city street range (12XX) to tigris street range (1200-1299)
street_ranges <- make_street_range(d_crime_by_street_range)
city_street_ranges <- left_join(city_streets, street_ranges, by = "ADDRESS_X")

# match street ranges
city_street_ranges$street_ranges <-
  purrr::pmap(city_street_ranges, 
              query_street_ranges, 
              .progress = "querying street ranges")

# reduce to one geometry per city street range
sf_street_range <- 
  unnest(city_street_ranges, cols = c(street_ranges)) |>
  filter(!is.na(tlid)) |>
  group_by(ADDRESS_X) |>
  summarize(tlid = paste(unique(tlid), collapse = "-"), 
            geometry = st_union(geometry)) |>
  st_as_sf()

# collapse tigris street ranges
sf_by_tigris_street_range <- 
  sf_street_range |>
  group_by(tlid, geometry) |>
  st_as_sf()

# merge back to incidents
d_crime <- d_crime_by_street_range |>
  left_join(sf_by_tigris_street_range, by = "ADDRESS_X") |>
  filter(!is.na(tlid)) |>
  filter(violent == 1 | property == 1) |>
  mutate(geometry = st_cast(geometry, "MULTILINESTRING"))

# write gpkg file 
st_write(d_crime, 
         "crime_incidents_with_street_range_2026_02_16.gpkg", 
         append = FALSE,
         delete_dsn = TRUE)
