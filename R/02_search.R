# Step 2: search each route's stop pattern one stop at a time, five seeds each, and record what the best one did.
# Run from the project folder:  Rscript R/02_search.R

suppressPackageStartupMessages({ library(sf); library(dplyr); library(readr) })
source("R/fns.R")

SEEDS <- 1:5
routes <- read_csv("data/processed/routes.csv", col_types = cols(route = "c", .default = col_guess()))
stops  <- st_read("data/processed/routes.gpkg", "stops", quiet = TRUE) |> st_drop_geometry()
blocks <- st_read("data/processed/blocks.gpkg", quiet = TRUE)
bxy    <- st_coordinates(blocks)
blocks <- data.frame(x = bxy[, 1], y = bxy[, 2], pop = blocks$POP20)
dir.create("output", showWarnings = FALSE)

summary_rows <- list()
for (r in routes$route) {
  info <- routes[routes$route == r, ]
  s <- stops[stops$route == r, ] |> arrange(seq)
  pad <- PARAMS$catchment_ft
  near <- blocks$x > min(s$x) - pad & blocks$x < max(s$x) + pad &
          blocks$y > min(s$y) - pad & blocks$y < max(s$y) + pad
  ctx <- build_ctx(s, blocks[near, ], info$trips_dir, info$riders_dir)

  t0 <- Sys.time()
  runs <- lapply(SEEDS, function(seed) climb(ctx, seed))
  secs <- as.numeric(Sys.time() - t0, units = "secs")
  scores <- vapply(runs, `[[`, numeric(1), "score")
  best <- runs[[which.max(scores)]]

  # The checks the plan promised: every move gained, the result is legal, fixed stops stayed.
  for (run in runs) stopifnot(all(diff(run$moves$score) > 0), is_valid(run$keep, ctx), all(run$keep[s$fixed]))

  c0 <- route_cost(rep(TRUE, nrow(s)), ctx)
  c1 <- route_cost(best$keep, ctx)
  length_ft <- max(s$dist_ft) - min(s$dist_ft)
  summary_rows[[r]] <- data.frame(
    route = r, name = info$name, direction = info$direction,
    riders_dir = info$riders_dir, trips_dir = info$trips_dir, market_pop = sum(ctx$pop),
    stops_before = nrow(s), stops_after = sum(best$keep), fixed = sum(s$fixed),
    spacing_before_ft = round(length_ft / (nrow(s) - 1)),
    spacing_after_ft = round(length_ft / (sum(best$keep) - 1)),
    max_gap_after_ft = round(max(diff(s$dist_ft[best$keep]))),
    stops_made_before = round(c0$stops_per_trip, 1), stops_made_after = round(c1$stops_per_trip, 1),
    runtime_saved_min = round((c0$stops_per_trip - c1$stops_per_trip) * PARAMS$penalty_s / 60, 1),
    walk_added_ft = round(c1$mean_walk_ft - c0$mean_walk_ft),
    ride_saved_pmin = round(c0$ride - c1$ride),
    walk_added_pmin = round(c1$walk - c0$walk),
    net_pmin = round(best$score),
    seed_min = round(min(scores)), seed_max = round(max(scores)),
    moves = nrow(best$moves) - 1, passes = best$passes
  )

  # Every accepted move of every seed, and the best seed's final stop list.
  log <- bind_rows(lapply(seq_along(runs), function(i) mutate(runs[[i]]$moves, seed = SEEDS[i]))) |>
    mutate(best = seed == SEEDS[which.max(scores)],
           stop_id = s$stop_id[stop], stop_name = s$stop_name[stop])
  write_csv(log, sprintf("output/search_log_%s.csv", r))
  write_csv(transmute(s, route = r, seq, stop_id, stop_name, dist_ft, fixed, fixed_why, kept = best$keep),
            sprintf("output/stops_%s.csv", r))
  cat(sprintf("route %-3s %3d -> %3d stops, net %s person-min/weekday, %.1fs\n",
              r, nrow(s), sum(best$keep), format(round(best$score), big.mark = ","), secs))
}

summ <- bind_rows(summary_rows)
write_csv(summ, "output/summary.csv")

fmt <- function(x) format(x, big.mark = ",")
md <- c("# 02_search run", "",
        sprintf("Parameters: %s.", paste(names(PARAMS), unlist(PARAMS), sep = " = ", collapse = ", ")),
        sprintf("Seeds: %s. Score: weekday person-minutes saved in one direction (walking counted at %sx).",
                paste(SEEDS, collapse = ", "), PARAMS$walk_weight), "",
        "| Route | Stops | Spacing (ft) | Stops made per trip | Runtime saved (min/trip) | Walk added (ft, mean) | Ride saved | Walk added | Net (person-min) | Seed range |",
        "|---|---|---|---|---|---|---|---|---|---|",
        with(summ, sprintf("| %s %s | %d to %d | %s to %s | %.1f to %.1f | %.1f | %d | %s | %s | %s | %s to %s |",
                           route, name, stops_before, stops_after, fmt(spacing_before_ft), fmt(spacing_after_ft),
                           stops_made_before, stops_made_after, runtime_saved_min, walk_added_ft,
                           fmt(ride_saved_pmin), fmt(walk_added_pmin), fmt(net_pmin), fmt(seed_min), fmt(seed_max))))
writeLines(md, "docs/runs/02_search.md")
cat("\n", md, sep = "\n")
