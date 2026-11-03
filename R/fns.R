# The model and the climb. Pure functions, no file reading; tests/ builds toy routes by hand.
# All distances are US feet (EPSG:3435), all costs are weekday person-minutes.

PARAMS <- list(
  penalty_s       = 20,    # time a bus loses per stop it makes: decel, accel, doors (not per-rider dwell)
  walk_ft_per_min = 264,   # 3 mph
  walk_weight     = 2,     # a minute walking feels like two minutes riding
  catchment_ft    = 1320,  # a block belongs to the route if a stop is within 1/4 mile (Manhattan)
  max_gap_ft      = 2640,  # never leave more than 1/2 mile between stops
  transfer_ft     = 300    # stops this close to a rail station or crossing route are fixed
)

# Walking distance on Chicago's grid, which lines up with the State Plane axes.
# Simplification: ignores the river, rail embankments and missing streets.
manhattan <- function(ax, ay, bx, by) {
  abs(outer(ax, bx, "-")) + abs(outer(ay, by, "-"))
}

# stops: data frame in route order with x, y, dist_ft (along the route), fixed.
# blocks: data frame with x, y, pop. trips: weekday trips in this direction. riders: weekday riders, this direction.
build_ctx <- function(stops, blocks, trips, riders, p = PARAMS) {
  stopifnot(!is.unsorted(stops$dist_ft), trips > 0, riders > 0)
  D <- manhattan(blocks$x, blocks$y, stops$x, stops$y)
  market <- apply(D, 1, min) <= p$catchment_ft
  list(stops = stops, D = D[market, , drop = FALSE], pop = blocks$pop[market],
       trips = trips, riders = riders)
}

# Cost of one stop pattern. keep: logical, one per stop.
route_cost <- function(keep, ctx, p = PARAMS) {
  k <- which(keep)
  near <- max.col(-ctx$D[, k, drop = FALSE], ties.method = "first")   # nearest kept stop per block
  walk_ft <- ctx$D[cbind(seq_along(near), k[near])]

  s <- tapply(ctx$pop, factor(near, levels = seq_along(k)), sum, default = 0)
  s <- as.numeric(s) / sum(ctx$pop)

  # Gravity O-D along the route: riders from j to a later k are proportional to s_j * s_k.
  # Simplification: demand is fixed, nobody gives up the bus over a longer walk.
  Z      <- (1 - sum(s^2)) / 2              # sum over j < k of s_j s_k
  before <- cumsum(s) - s
  after  <- 1 - cumsum(s)
  load   <- ctx$riders * before * after / Z # riders on board through the stop
  ons_offs <- ctx$riders * s * (1 - s) / Z

  p_stop <- 1 - exp(-ons_offs / ctx$trips)  # Poisson: a stop with no one waiting or getting off is skipped
  ride <- sum(p_stop * load) * p$penalty_s / 60
  mean_walk_ft <- sum(ctx$pop * walk_ft) / sum(ctx$pop)
  walk <- 2 * ctx$riders * mean_walk_ft / p$walk_ft_per_min   # both ends of every trip

  list(total = ride + p$walk_weight * walk, ride = ride, walk = walk,
       mean_walk_ft = mean_walk_ft, stops_per_trip = sum(p_stop))
}

# Fixed stops stay, and removing stops never opens a gap over max_gap_ft.
# A gap that already existed in the schedule (no stop removed inside it) is allowed.
is_valid <- function(keep, ctx, p = PARAMS) {
  if (!all(keep[ctx$stops$fixed])) return(FALSE)
  k <- which(keep)
  spans_removed <- diff(k) > 1
  all(diff(ctx$stops$dist_ft[k])[spans_removed] <= p$max_gap_ft)
}

# First-improvement local search from the current schedule: toggle one stop at a time in random
# order, keep the toggle if it is valid and the score rises, stop after a pass with no gain.
# Score = baseline cost - cost, so the schedule as run today scores 0.
climb <- function(ctx, seed = 1, p = PARAMS) {
  set.seed(seed)
  keep <- rep(TRUE, nrow(ctx$stops))
  base <- route_cost(keep, ctx, p)$total
  best <- 0
  movable <- which(!ctx$stops$fixed)
  moves <- list(data.frame(iter = 0L, pass = 0L, stop = NA_integer_, action = "start", score = 0))
  pass <- 0L
  repeat {
    pass <- pass + 1L
    improved <- FALSE
    for (i in movable[sample.int(length(movable))]) {
      keep[i] <- !keep[i]
      if (is_valid(keep, ctx, p)) {
        score <- base - route_cost(keep, ctx, p)$total
        if (score > best + 1e-9) {
          best <- score
          improved <- TRUE
          moves[[length(moves) + 1]] <- data.frame(
            iter = length(moves), pass = pass, stop = i,
            action = if (keep[i]) "restore" else "remove", score = score)
          next
        }
      }
      keep[i] <- !keep[i]   # undo
    }
    if (!improved) break
  }
  list(keep = keep, score = best, moves = do.call(rbind, moves), passes = pass)
}
