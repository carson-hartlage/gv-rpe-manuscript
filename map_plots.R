library(tidycensus)
library(sf)
library(ggspatial)

hamilton_blocks <- get_decennial(
  geography = "block group",
  variables = c(total_pop = "P1_001N"),
  year = 2020,
  state = "OH",
  county = "Hamilton",
  geometry = TRUE
)

city <- cincy::neigh_sna |>
  st_union() |>
  st_transform(st_crs(hamilton_blocks))

blocks_overlap <- hamilton_blocks |>
  st_intersection(city) |>
  group_by(GEOID) |>
  summarise(
    total_pop = dplyr::first(value),
    .groups = "drop"
  )

g <- 
  read.csv("~/gun-violence-incidents-hamilton.csv") |>
  mutate(incident_date = as.Date(date)) |>
  filter(date >= "2016-01-01" & date <= "2024-12-31") |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
  st_transform(st_crs(blocks_overlap))
g_in <- g |>
  st_join(blocks_overlap["GEOID"], join = st_within) |>
  filter(!is.na(GEOID))
g_counts <- g_in |>
  st_drop_geometry() |>
  count(GEOID, name = "incidents")
pop_df <- blocks_overlap |>
  st_drop_geometry() |>
  dplyr::select(GEOID, pop = total_pop)

rates <- pop_df |>
  left_join(g_counts, by = "GEOID") |>
  mutate(
    incidents = if_else(is.na(incidents), 0L, incidents),
    rate_per_100k = (incidents / pop) * 1000
  )
rates$rate <- with(rates, ifelse(
  pop > 0,
  (incidents / pop) * 1000,
  NA
))
map_df <- blocks_overlap |>
  left_join(rates, by = "GEOID") |>
  mutate(
    rate_per_100k = if_else(is.na(rate_per_100k), 0, rate_per_100k)
  )

p2 <- ggplot() +
  geom_sf(data = map_df,
          aes(fill = rate_per_100k),
          color = NA) +
  geom_sf(data = city,
          fill = NA,
          color = "black",
          linewidth = 0.4) +
  annotation_scale(
    location = "bl",
    style = "ticks",
    width_hint = 0.2,
    line_width = 1.2,
    text_cex = 0.75
  ) +
  scale_fill_viridis_c(
    option = "magma",
    trans = "log1p",
    name = "Incidents per 1000",
    breaks = c(0, 2, 5, 10, 25, 50, 100),
    labels = scales::label_number()
  ) +
  coord_sf() +
  theme_void()


#### FIGURE 1C
c <- readRDS("~/linked_addresses.rds") |>
  slice_min(order_by = imputed_start_date, n = 1, by = c(MRN), with_ties = FALSE) |>
  st_transform(crs = st_crs(map_df)) |>
  st_join(blocks_overlap["GEOID"], join = st_within) |>
  group_by(MRN) |>
  slice_sample(n=1) |>
  ungroup()

pt_counts <- c |>
  group_by(GEOID) |>
  summarise(n_pts = n()) |>
  ungroup()

rates <- pop_df |>
  left_join(pt_counts, by = "GEOID") |>
  mutate(
    n_pts = if_else(is.na(n_pts), 0L, n_pts),
    rate_per_100k = (n_pts / pop) * 1000
  )
rates$rate <- with(rates, ifelse(
  pop > 0,
  (n_pts / pop) * 1000,
  NA
))

map_df <- blocks_overlap |>
  left_join(rates, by = "GEOID") |>
  mutate(
    rate_per_100k = if_else(is.na(rate_per_100k), 0, rate_per_100k)
  )

p1 <- ggplot() +
  geom_sf(data = map_df,
          aes(fill = rate_per_100k),
          color = NA) +
  geom_sf(data = city,
          fill = NA,
          color = "black",
          linewidth = 0.4) +
  annotation_scale(
    location = "bl",
    style = "ticks",
    width_hint = 0.2,
    line_width = 1.2,
    text_cex = 0.75
  ) +
  scale_fill_viridis_c(
    option = "magma",
    trans = "log1p",
    name = "Patients per 1,000",
    breaks = c(0, 2, 5, 10, 20, 40, 60),
    labels = scales::label_number(accuracy = 1)
  ) +
  coord_sf() +
  theme_void()

###
# all Cincinnati residential addresses
all <- 
  readRDS("~/data_for_analysis_hosp_20250331.rds") |>
  mutate(census_tract_id_2020 = str_sub(census_block_group_id_2020, end = -2)) |>
  #filter(census_tract_id_2020 == "39061006800") |>
  st_as_sf(coords = c("centroid_lon", "centroid_lat"), crs = "WGS84") |>
  st_transform(crs="NAD83")

all <- st_transform(all, 26917)
g   <- st_transform(g, 26917)

sample <- all |> slice_sample(n = 1)
#sample <- filter(all, cagis_address== "3253 HANNA AV CINCINNATI, OH 45211")

addresses_buffer <- st_buffer(sample, dist = 400)

pts <- g[st_intersects(g, addresses_buffer, sparse = FALSE), ]
pts$label <- format(pts$incident_date, "%Y-%m-%d")
print(nrow(pts))

parcel_shapes <- 
  st_read(
    dsn = "/vsizip/vsicurl/https://www.cagis.org/Opendata/Auditor/HAM_PARCELS.gdb.zip"
  ) |>
  st_zm() |>
  dplyr::select(parcel_id = AUDPTYID) |>
  filter(!is.na(parcel_id))

bb <- st_bbox(st_buffer(sample, 500))

bb_local <- st_transform(
  st_as_sfc(bb),
  st_crs(parcel_shapes)
)

parcel_shapes_bb <- parcel_shapes |>
  st_cast("MULTIPOLYGON") |>
  st_crop(bb_local) |>
  st_transform(crs = st_crs(sample))

ggplot() +
  geom_sf(data = parcel_shapes_bb,
          fill = NA,
          linewidth = 0.3,
          color = "gray90",
          alpha = 0.8) +
  geom_sf(data = addresses_buffer,
          fill = "dodgerblue",
          alpha = 0.2,
          color = NA) +
  geom_sf(data = pts,
          color = "#E41A1C",
          size = 2) +
  geom_label_repel(
    data = cbind(pts, st_coordinates(pts)),
    aes(X, Y, label = label),
    size = 2.8,
    fill = "#f5bcbd",
    linewidth = 0
  ) +
  geom_sf(data = sample,
          shape = 21,
          fill = "blue",
          color = "black",
          size = 3) +
  coord_sf(
    xlim = c(bb["xmin"], bb["xmax"]),
    ylim = c(bb["ymin"], bb["ymax"]),
    expand = FALSE
  ) +
  theme_void() +
  annotation_scale(
    location = "bl",
    style = "ticks",
    width_hint = 0.2,
    line_width = 1.2,
    text_cex = 0.75
  )
