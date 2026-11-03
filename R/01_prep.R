# Step 1: turn the CTA schedule, route ridership and 2020 Census blocks into the small input set
# in data/processed/. Run from the project folder:  Rscript R/01_prep.R
#
# Downloads what it needs into data/raw/ on the first run (about 380 MB, mostly Census blocks).
# data/processed/ is committed, so steps 2 to 4 run without this step.

suppressPackageStartupMessages({ library(sf); library(dplyr); library(readr) })
source("R/fns.R")

ROUTES     <- c("79", "66", "9", "53", "8")   # busiest weekday routes, Jul 2025 to Jun 2026
RIDES_FROM <- "2025-07-01"                    # ridership window: the last 12 months on record
RIDES_TO   <- "2026-06-30"
CRS        <- 3435                            # IL State Plane East, US feet
RAW        <- "data/raw"
GTFS       <- file.path(RAW, "cta_gtfs.zip")
RIDES      <- file.path(RAW, "cta_bus_ridership.json")
BLOCKS_ZIP <- file.path(RAW, "tl_2020_17_tabblock20.zip")
BLOCKS     <- file.path(RAW, "cook_blocks.gpkg")
dir.create(RAW, showWarnings = FALSE, recursive = TRUE)
dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)
options(timeout = 3600)

fetch <- function(url, dest) {
  if (!file.exists(dest)) {
    message("Downloading ", url)
    download.file(url, dest, mode = "wb", quiet = TRUE)
  }
}

# The CTA feed changes every few weeks; the committed results use the feed published 2026-09-14.
fetch("https://www.transitchicago.com/downloads/sch_data/google_transit.zip", GTFS)
fetch(paste0("https://data.cityofchicago.org/resource/jyb9-n7fm.json?", utils::URLencode(paste0(
  "$select=route,date,daytype,rides&$limit=50000&$where=daytype='W' AND route in('",
  paste(ROUTES, collapse = "','"), "') AND date between '", RIDES_FROM, "T00:00:00' and '", RIDES_TO, "T23:59:59'"))),
  RIDES)

gtfs <- function(file) read_csv(unz(GTFS, file), col_types = cols(.default = "c"), progress = FALSE)

# Blocks: Cook County, people only, one point per block --------------------------------------
if (!file.exists(BLOCKS)) {
  fetch("https://www2.census.gov/geo/tiger/TIGER2020/TABBLOCK20/tl_2020_17_tabblock20.zip", BLOCKS_ZIP)
  cook <- st_read(paste0("/vsizip/", BLOCKS_ZIP, "/tl_2020_17_tabblock20.shp"), quiet = TRUE,
                  query = "SELECT GEOID20, POP20 FROM tl_2020_17_tabblock20 WHERE COUNTYFP20 = '031'")
  cook_pop <- sum(cook$POP20)
  stopifnot(abs(cook_pop - 5275541) / 5275541 < 0.001)   # 2020 census count for Cook County
  cook |>
    filter(POP20 > 0) |>
    st_transform(CRS) |>
    st_point_on_surface() |>
    st_write(BLOCKS, quiet = TRUE)
}
blocks <- st_read(BLOCKS, quiet = TRUE)

# Service day: a Wednesday in the feed ---------------------------------------------------------
calendar <- gtfs("calendar.txt")
cal_dates <- gtfs("calendar_dates.txt")
days <- seq(as.Date(min(calendar$start_date), "%Y%m%d"), as.Date(max(calendar$end_date), "%Y%m%d"), by = 1)
day <- days[format(days, "%u") == "3"][1]
dstr <- format(day, "%Y%m%d")
services <- union(
  calendar$service_id[calendar$wednesday == "1" & calendar$start_date <= dstr & calendar$end_date >= dstr],
  cal_dates$service_id[cal_dates$date == dstr & cal_dates$exception_type == "1"]
)
services <- setdiff(services, cal_dates$service_id[cal_dates$date == dstr & cal_dates$exception_type == "2"])

routes <- gtfs("routes.txt")
bus_routes <- routes$route_id[routes$route_type == "3"]
trips <- gtfs("trips.txt") |> filter(service_id %in% services, route_id %in% bus_routes)

# One representative trip per shape, for every bus route (crossing routes need theirs too).
rep_trips <- trips |> group_by(route_id, direction_id, shape_id) |>
  summarise(n = n(), trip_id = first(trip_id), .groups = "drop")

# Each target route: the direction and shape with the most trips. Trips counted are all trips that
# day in that direction. Simplification: short-turn trips count as full length.
patterns <- rep_trips |> filter(route_id %in% ROUTES) |>
  group_by(route_id) |> slice_max(n, n = 1, with_ties = FALSE) |> ungroup() |>
  left_join(trips |> count(route_id, direction_id, name = "trips_dir"), by = c("route_id", "direction_id")) |>
  left_join(trips |> distinct(route_id, direction_id, direction), by = c("route_id", "direction_id"))
stopifnot(setequal(patterns$route_id, ROUTES))

# stop_times.txt is 367 MB: stream it and keep only the representative trips.
keep_ids <- rep_trips$trip_id
st <- read_csv_chunked(unz(GTFS, "stop_times.txt"),
                       DataFrameCallback$new(function(x, pos) x[x$trip_id %in% keep_ids, ]),
                       chunk_size = 1e6, col_types = cols(.default = "c"), progress = FALSE)

stops_all <- gtfs("stops.txt") |>
  st_as_sf(coords = c("stop_lon", "stop_lat"), crs = 4326) |> st_transform(CRS)
xy <- st_coordinates(stops_all)
stops_all$x <- xy[, 1]
stops_all$y <- xy[, 2]
stations <- stops_all |> filter(location_type == "1")

# Every bus pattern's stops in order, with the street bearing at each stop (0 to 180 degrees).
pattern_stops <- st |>
  inner_join(rep_trips |> select(trip_id, route_id, shape_id), by = "trip_id") |>
  mutate(seq = as.integer(stop_sequence), dist_ft = as.numeric(shape_dist_traveled)) |>
  arrange(route_id, shape_id, seq) |>
  left_join(st_drop_geometry(stops_all) |> select(stop_id, stop_name, x, y), by = "stop_id") |>
  group_by(route_id, shape_id) |>
  mutate(bearing = (atan2(lead(y, default = last(y)) - lag(y, default = first(y)),
                         lead(x, default = last(x)) - lag(x, default = first(x))) * 180 / pi) %% 180) |>
  ungroup()

# Two directions cross if their streets meet at 45 to 135 degrees.
crossing <- function(a, b) { d <- abs(a - b) %% 180; d > 45 & d < 135 }

route_stops <- lapply(seq_len(nrow(patterns)), function(r) {
  pat <- patterns[r, ]
  s <- pattern_stops |> filter(shape_id == pat$shape_id) |> mutate(route = pat$route_id)
  others <- pattern_stops |> filter(route_id != pat$route_id)

  d_rail <- sqrt(outer(s$x, stations$x, "-")^2 + outer(s$y, stations$y, "-")^2)
  d_bus  <- sqrt(outer(s$x, others$x, "-")^2 + outer(s$y, others$y, "-")^2)
  cross  <- outer(s$bearing, others$bearing, crossing)
  near_rail <- apply(d_rail <= PARAMS$transfer_ft, 1, any)
  near_bus  <- apply(d_bus <= PARAMS$transfer_ft & cross, 1, any)
  # Name the crossing routes, so each fixed stop says why it is fixed.
  cross_routes <- apply(d_bus <= PARAMS$transfer_ft & cross, 1, function(m) paste(sort(unique(others$route_id[m])), collapse = " "))

  terminal <- seq_len(nrow(s)) %in% c(1, nrow(s))
  s |> mutate(
    fixed = terminal | near_rail | near_bus,
    fixed_why = case_when(terminal ~ "terminal", near_rail ~ "rail",
                          near_bus ~ paste("bus", cross_routes), TRUE ~ "")
  )
}) |> bind_rows()

# Ridership: weekday mean over the window, half per direction ------------------------------------
rides <- jsonlite::fromJSON(RIDES) |>
  filter(daytype == "W", route %in% ROUTES, date >= RIDES_FROM, date <= paste0(RIDES_TO, "T23:59:59")) |>
  group_by(route) |> summarise(riders_day = mean(as.numeric(rides)), days = n())
stopifnot(setequal(rides$route, ROUTES), all(rides$days > 200))

route_table <- patterns |>
  left_join(routes |> select(route_id, route_long_name), by = "route_id") |>
  left_join(rides, by = c("route_id" = "route")) |>
  left_join(route_stops |> group_by(route) |>
              summarise(n_stops = n(), n_fixed = sum(fixed), length_ft = max(dist_ft) - min(dist_ft)),
            by = c("route_id" = "route")) |>
  transmute(route = route_id, name = route_long_name, direction, shape_id, service_date = format(day),
            trips_dir, riders_day = round(riders_day), riders_dir = round(riders_day / 2),
            n_stops, n_fixed, length_ft = round(length_ft))
stopifnot(all(route_table$n_stops >= 50),
          !any(tapply(route_stops$dist_ft, route_stops$route, is.unsorted)))

# Route lines from shapes.txt, for the maps.
shape_lines <- gtfs("shapes.txt") |>
  filter(shape_id %in% patterns$shape_id) |>
  mutate(seq = as.integer(shape_pt_sequence)) |> arrange(shape_id, seq) |>
  st_as_sf(coords = c("shape_pt_lon", "shape_pt_lat"), crs = 4326) |>
  group_by(shape_id) |> summarise(do_union = FALSE) |> st_cast("LINESTRING") |>
  st_transform(CRS) |>
  left_join(patterns |> select(shape_id, route = route_id), by = "shape_id")

out <- "data/processed/routes.gpkg"
unlink(out)
route_stops |>
  select(route, seq, stop_id, stop_name, dist_ft, x, y, fixed, fixed_why) |>
  st_as_sf(coords = c("x", "y"), crs = CRS, remove = FALSE) |>
  st_write(out, layer = "stops", quiet = TRUE)
st_write(shape_lines, out, layer = "shapes", quiet = TRUE, append = TRUE)
write_csv(route_table, "data/processed/routes.csv")

# Only the blocks some route could serve, so the committed input stays small.
bxy <- st_coordinates(blocks)
served <- Reduce(`|`, lapply(split(route_stops, route_stops$route), function(s) {
  d <- manhattan(bxy[, 1], bxy[, 2], s$x, s$y)
  apply(d, 1, min) <= PARAMS$catchment_ft
}))
out_blocks <- "data/processed/blocks.gpkg"
unlink(out_blocks)
st_write(blocks[served, c("GEOID20", "POP20")], out_blocks, quiet = TRUE)

# Run notes -----------------------------------------------------------------------------------
dir.create("docs/runs", showWarnings = FALSE, recursive = TRUE)
md <- c("# 01_prep run", "",
        sprintf("Service day: %s. Cook County blocks with people: %s (%s residents).",
                format(day), format(nrow(blocks), big.mark = ","), format(sum(blocks$POP20), big.mark = ",")), "",
        "| Route | Name | Direction | Weekday trips (dir) | Riders/day | Stops | Fixed | Length (mi) |",
        "|---|---|---|---|---|---|---|---|",
        with(route_table, sprintf("| %s | %s | %s | %d | %s | %d | %d | %.1f |", route, name, direction,
                                  trips_dir, format(riders_day, big.mark = ","), n_stops, n_fixed, length_ft / 5280)))
writeLines(md, "docs/runs/01_prep.md")
cat(md, sep = "\n")
