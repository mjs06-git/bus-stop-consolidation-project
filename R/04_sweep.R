# Step 4: how much do the results lean on the two judgment calls, walk_weight and penalty_s?
# Reruns the climb (5 seeds, best kept) for every pair on a 3 x 3 grid, all five routes.
# Run from the project folder after 01:  Rscript R/04_sweep.R

suppressPackageStartupMessages({ library(sf); library(dplyr); library(readr); library(ggplot2) })
source("R/fns.R")

SEEDS <- 1:5
GRID <- expand.grid(walk_weight = c(1.5, 2, 2.5), penalty_s = c(15, 20, 25))
routes <- read_csv("data/processed/routes.csv", col_types = cols(route = "c", .default = col_guess()))
stops  <- st_read("data/processed/routes.gpkg", "stops", quiet = TRUE) |> st_drop_geometry()
blocks <- st_read("data/processed/blocks.gpkg", quiet = TRUE)
bxy    <- st_coordinates(blocks)
blocks <- data.frame(x = bxy[, 1], y = bxy[, 2], pop = blocks$POP20)

rows <- list()
for (r in routes$route) {
  info <- routes[routes$route == r, ]
  s <- stops[stops$route == r, ] |> arrange(seq)
  pad <- PARAMS$catchment_ft
  near <- blocks$x > min(s$x) - pad & blocks$x < max(s$x) + pad &
          blocks$y > min(s$y) - pad & blocks$y < max(s$y) + pad
  ctx <- build_ctx(s, blocks[near, ], info$trips_dir, info$riders_dir)
  length_ft <- max(s$dist_ft) - min(s$dist_ft)

  for (g in seq_len(nrow(GRID))) {
    p <- modifyList(PARAMS, as.list(GRID[g, ]))
    runs <- lapply(SEEDS, function(seed) climb(ctx, seed, p))
    best <- runs[[which.max(vapply(runs, `[[`, numeric(1), "score"))]]
    stopifnot(is_valid(best$keep, ctx, p))
    c0 <- route_cost(rep(TRUE, nrow(s)), ctx, p)
    c1 <- route_cost(best$keep, ctx, p)
    rows[[length(rows) + 1]] <- data.frame(
      route = r, name = info$name, walk_weight = p$walk_weight, penalty_s = p$penalty_s,
      stops_before = nrow(s), stops_after = sum(best$keep), removed = nrow(s) - sum(best$keep),
      spacing_after_ft = round(length_ft / (sum(best$keep) - 1)),
      runtime_saved_min = round((c0$stops_per_trip - c1$stops_per_trip) * p$penalty_s / 60, 1),
      walk_added_ft = round(c1$mean_walk_ft - c0$mean_walk_ft),
      net_pmin = round(best$score))
  }
  cat("route", r, "done\n")
}
sweep <- bind_rows(rows)
write_csv(sweep, "output/sweep.csv")

# Chart: one panel per route, walk weight along x, one line per stop penalty (labelled in the first panel).
order <- sweep |> filter(walk_weight == 2, penalty_s == 20) |> arrange(desc(net_pmin))
sweep <- mutate(sweep, label = factor(paste(route, name), paste(order$route, order$name)),
                penalty = factor(paste(penalty_s, "s"), paste(c(25, 20, 15), "s")))
PENALTY_COLOR <- c(`25 s` = "#2a78d6", `20 s` = "#eb6834", `15 s` = "#1baf7a")   # first three categorical slots
p <- ggplot(sweep, aes(walk_weight, removed / stops_before, color = penalty)) +
  geom_line(linewidth = 0.7) + geom_point(size = 2) +
  geom_text(data = filter(sweep, walk_weight == 2.5, label == levels(label)[1]), aes(label = penalty), hjust = 0, nudge_x = 0.08,
            size = 3.2, color = "#52514e", show.legend = FALSE) +
  facet_wrap(~label, nrow = 1) +
  scale_x_continuous(breaks = c(1.5, 2, 2.5), labels = function(x) paste0(x, "x"),
                     expand = expansion(add = c(0.1, 0.45))) +
  scale_y_continuous(labels = scales::label_percent(), limits = c(0, NA)) +
  scale_color_manual(values = PENALTY_COLOR, name = "Time a stop costs riders on board") +
  labs(x = "A minute of walking counted as this many minutes of riding", y = "Share of stops removed",
       title = "Pulaski and Halsted lead under every assumption",
       subtitle = "Stops removed for each pair of assumptions, best of 5 seeds. The main result is 20 s and 2x.") +
  theme_minimal(base_size = 11) +
  theme(text = element_text(color = "#0b0b0b"), plot.title = element_text(face = "bold", size = 13),
        plot.subtitle = element_text(color = "#52514e"), plot.title.position = "plot",
        legend.position = "top", legend.justification = "left", panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(), panel.grid.major.y = element_line(color = "#e8e7e3"),
        strip.text = element_text(face = "bold", hjust = 0))
ggsave("output/sweep.png", p, width = 12, height = 4.2, dpi = 160, bg = "white")

# Run notes: stops removed and net person-minutes, routes down, grid across.
cell <- function(df) sprintf("%d (%s)", df$removed, prettyNum(df$net_pmin, big.mark = ","))
head_cells <- with(GRID, sprintf("w %.1f, %d s", walk_weight, penalty_s))
md <- c("# 04_sweep run", "",
        "Stops removed, with net weekday person-minutes in brackets. w = walk_weight, s = penalty_s.", "",
        paste0("| Route | ", paste(head_cells, collapse = " | "), " |"),
        paste0("|---|", strrep("---|", nrow(GRID))),
        vapply(routes$route, function(r) {
          d <- sweep[sweep$route == r, ]
          d <- d[match(paste(GRID$walk_weight, GRID$penalty_s), paste(d$walk_weight, d$penalty_s)), ]
          paste0("| ", r, " ", d$name[1], " | ", paste(cell(d), collapse = " | "), " |")
        }, character(1)))
writeLines(md, "docs/runs/04_sweep.md")
cat("\n", md, sep = "\n")
