# Step 3: figures. Run from the project folder after step 2:  Rscript R/03_plot.R
#   output/overview.png  all five routes on one map
#   output/stops.png     each route as a straight line, stop by stop
#   output/search.png    the score after each accepted move, every seed

suppressPackageStartupMessages({ library(sf); library(dplyr); library(readr); library(ggplot2) })
source("R/fns.R")

# Stop status: grey for kept, orange X for removed, near-black square for fixed. Shape repeats
# the color, so the figures read in grayscale and for colorblind readers.
STATUS_COLOR <- c(Kept = "#a3a29c", Removed = "#eb6834", Fixed = "#2b2b29")
STATUS_SHAPE <- c(Kept = 16, Removed = 4, Fixed = 15)
INK <- "#0b0b0b"; INK_2 <- "#52514e"; GRID <- "#e8e7e3"

routes <- read_csv("data/processed/routes.csv", col_types = cols(route = "c", .default = col_guess()))
summ   <- read_csv("output/summary.csv", col_types = cols(route = "c", .default = col_guess())) |>
  arrange(desc(net_pmin)) |>
  mutate(label = paste(route, name))
shapes <- st_read("data/processed/routes.gpkg", "shapes", quiet = TRUE)
blocks <- st_read("data/processed/blocks.gpkg", quiet = TRUE)
stops <- bind_rows(lapply(routes$route, function(r)
  read_csv(sprintf("output/stops_%s.csv", r), col_types = cols(route = "c", stop_id = "c", .default = col_guess())))) |>
  mutate(status = factor(case_when(fixed ~ "Fixed", kept ~ "Kept", TRUE ~ "Removed"), names(STATUS_COLOR)),
         label = factor(summ$label[match(route, summ$route)], rev(summ$label)))
stops_sf <- st_read("data/processed/routes.gpkg", "stops", quiet = TRUE) |>
  select(route, stop_id) |> left_join(stops, by = c("route", "stop_id"))

theme_set(theme_minimal(base_size = 11) + theme(
  text = element_text(color = INK), plot.title = element_text(face = "bold", size = 13),
  plot.subtitle = element_text(color = INK_2), plot.caption = element_text(color = INK_2, hjust = 0),
  plot.title.position = "plot", plot.caption.position = "plot",
  axis.text = element_text(color = INK_2), panel.grid = element_blank(),
  legend.position = "top", legend.justification = "left", legend.title = element_blank()))

# Overview map ------------------------------------------------------------------------------------
# Label each route above its north end (north-south routes) or below its west end (east-west routes).
ends <- bind_rows(lapply(seq_len(nrow(shapes)), function(i) {
  xy <- st_coordinates(shapes[i, ])
  ns <- diff(range(xy[, "Y"])) > diff(range(xy[, "X"]))
  end <- if (ns) xy[which.max(xy[, "Y"]), ] else xy[which.min(xy[, "X"]), ]
  data.frame(route = shapes$route[i], x = end[["X"]],
             y = end[["Y"]] + if (ns) 1800 else -3600, hjust = if (ns) 0.5 else 0)
})) |> mutate(label = paste(route, summ$name[match(route, summ$route)]))
overview <- ggplot() +
  geom_sf(data = blocks, aes(size = POP20), color = "#d9d8d3", stroke = 0, alpha = 0.7) +
  geom_sf(data = shapes, color = "#8a8983", linewidth = 0.35) +
  geom_sf(data = filter(stops_sf, status != "Removed"), aes(color = status, shape = status), size = 0.9) +
  geom_sf(data = filter(stops_sf, status == "Removed"), aes(color = status, shape = status), size = 1.8, stroke = 0.9) +
  geom_text(data = ends, aes(x, y, label = label, hjust = hjust), size = 3.4, color = INK, fontface = "bold") +
  scale_color_manual(values = STATUS_COLOR, drop = FALSE) +
  scale_shape_manual(values = STATUS_SHAPE, drop = FALSE) +
  scale_size_area(max_size = 1.6, guide = "none") +
  guides(color = guide_legend(override.aes = list(size = 2.5))) +
  labs(title = "Stops a stop-by-stop search would remove",
       subtitle = sprintf("Five busiest CTA bus routes: %d of %d stops removed, one direction each.",
                          sum(summ$stops_before - summ$stops_after), sum(summ$stops_before)),
       caption = paste("Grey dots: populated 2020 Census blocks within 1/4 mile of a stop, sized by population.",
                       "Fixed: terminals and transfers to rail or crossing bus routes.", sep = "\n"),
       x = NULL, y = NULL) +
  theme(axis.text = element_blank())
ggsave("output/overview.png", overview, width = 8, height = 9.5, dpi = 160, bg = "white")

# Stops along each route --------------------------------------------------------------------------
right <- summ |> transmute(label = factor(label, levels(stops$label)),
                           x = max(stops$dist_ft) / 5280 + 0.5,
                           text = sprintf("%d to %d stops, %.1f min faster", stops_before, stops_after, runtime_saved_min))
strip <- ggplot(stops, aes(dist_ft / 5280, label)) +
  geom_line(aes(group = label), color = GRID, linewidth = 0.8) +
  geom_point(data = filter(stops, status != "Removed"), aes(color = status, shape = status), size = 1.8) +
  geom_point(data = filter(stops, status == "Removed"), aes(color = status, shape = status), size = 2.4, stroke = 1) +
  geom_text(data = right, aes(x, label, label = text), hjust = 0, size = 3.3, color = INK_2) +
  scale_color_manual(values = STATUS_COLOR, drop = FALSE) +
  scale_shape_manual(values = STATUS_SHAPE, drop = FALSE) +
  scale_x_continuous(breaks = seq(0, 15, 5), expand = expansion(mult = c(0.01, 0))) +
  coord_cartesian(xlim = c(0, max(stops$dist_ft) / 5280 + 6.2), clip = "off") +
  labs(title = "Which stops go, route by route",
       subtitle = "Each line is one route in its busiest direction. Routes ordered by person-minutes saved.",
       x = "Miles from the start of the route", y = NULL) +
  theme(axis.text.y = element_text(color = INK, size = 11), axis.ticks.x = element_line(color = GRID))
ggsave("output/stops.png", strip, width = 12, height = 4.2, dpi = 160, bg = "white")

# Search progress ---------------------------------------------------------------------------------
log <- bind_rows(lapply(routes$route, function(r)
  read_csv(sprintf("output/search_log_%s.csv", r), col_types = cols(.default = col_guess())) |> mutate(route = r))) |>
  mutate(label = factor(summ$label[match(route, summ$route)], summ$label))
finals <- log |> group_by(label, seed) |> slice_max(iter, n = 1) |> ungroup()
search <- ggplot(log, aes(iter, score, group = seed)) +
  geom_step(data = filter(log, !best), color = "#c3c2b7", linewidth = 0.5) +
  geom_step(data = filter(log, best), color = "#2a78d6", linewidth = 0.9) +
  geom_point(data = filter(finals, !best), color = "#8a8983", size = 1.4) +
  geom_point(data = filter(finals, best), color = "#2a78d6", size = 1.8) +   # on top when seeds tie
  facet_wrap(~label, scales = "free", nrow = 1) +
  scale_x_continuous(breaks = function(l) unique(floor(pretty(l)))) +
  scale_y_continuous(labels = scales::label_comma(), limits = c(0, NA)) +
  labs(title = "Every accepted move raises the score",
       subtitle = "Five random visiting orders per route; the best one in blue, dots where each stopped.",
       x = "Accepted move", y = "Person-minutes saved per weekday") +
  theme(panel.grid.major.y = element_line(color = GRID), strip.text = element_text(face = "bold", hjust = 0))
ggsave("output/search.png", search, width = 12, height = 3.8, dpi = 160, bg = "white")

cat("Wrote output/overview.png, output/stops.png, output/search.png\n")
