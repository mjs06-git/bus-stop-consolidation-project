# Run from the project folder:  Rscript -e 'testthat::test_dir("tests")'
# No real data needed: each test builds a toy route by hand along the x axis.

source(testthat::test_path("..", "R", "fns.R"))

toy_stops <- function(x, fixed = c(TRUE, rep(FALSE, length(x) - 2), TRUE)) {
  data.frame(x = x, y = 0, dist_ft = x, fixed = fixed)
}

test_that("route_cost matches a three-stop route worked by hand", {
  # One block of 100 people on each stop: nobody walks, each stop has a third of the demand.
  ctx <- build_ctx(toy_stops(c(0, 1000, 2000)),
                   data.frame(x = c(0, 1000, 2000), y = 0, pop = 100),
                   trips = 100, riders = 300)
  c0 <- route_cost(c(TRUE, TRUE, TRUE), ctx)
  # O-D pairs 1-2, 1-3, 2-3 get 100 riders each. Through the middle: only 1-3 (100 riders).
  # Ons and offs at the middle: 200 a day, 2 per trip, so the bus stops there 1 - exp(-2) of the time.
  # The terminals carry no through riders, so they cost nothing.
  expect_equal(c0$ride, (1 - exp(-2)) * 100 * 20 / 60)
  expect_equal(c0$walk, 0)
  expect_equal(c0$stops_per_trip, 3 * (1 - exp(-2)))   # every stop has 200 ons and offs a day
})

test_that("walk cost counts both ends of every trip", {
  # Blocks 264 ft off the route: one minute's walk at each end.
  ctx <- build_ctx(toy_stops(c(0, 1000, 2000)),
                   data.frame(x = c(0, 1000, 2000), y = 264, pop = 100),
                   trips = 100, riders = 300)
  c0 <- route_cost(c(TRUE, TRUE, TRUE), ctx)
  expect_equal(c0$walk, 2 * 300 * 1)
  expect_equal(c0$total, c0$ride + 2 * c0$walk)
})

test_that("blocks beyond the catchment are not part of the route's market", {
  ctx <- build_ctx(toy_stops(c(0, 1000)),
                   data.frame(x = c(0, 0), y = c(100, 5000), pop = 100),
                   trips = 10, riders = 10)
  expect_equal(nrow(ctx$D), 1)
})

# Five stops: two only 200 ft apart (1000 and 1200), and a lone one at 2400.
five <- toy_stops(c(0, 1000, 1200, 2400, 3600))
# A block halfway between the close pair loses nothing when either one goes.
blocks <- data.frame(x = c(0, 600, 1100, 1800, 2400, 3000, 3600), y = 200, pop = 100)
tight <- modifyList(PARAMS, list(max_gap_ft = 1500))

test_that("is_valid rejects a gap over max_gap_ft and a removed fixed stop", {
  ctx <- build_ctx(five, blocks, trips = 50, riders = 5000)
  expect_true(is_valid(c(TRUE, FALSE, TRUE, TRUE, TRUE), ctx, tight))
  expect_false(is_valid(c(TRUE, TRUE, TRUE, FALSE, TRUE), ctx, tight))   # 1200 to 3600 is 2400 ft
  expect_false(is_valid(c(FALSE, TRUE, TRUE, TRUE, TRUE), ctx, tight))   # terminal
})

test_that("a gap already in the schedule is not held against the route", {
  ctx <- build_ctx(toy_stops(c(0, 3000, 3500, 4000)), blocks, trips = 50, riders = 5000)
  expect_true(is_valid(c(TRUE, TRUE, FALSE, TRUE), ctx, tight))
})

test_that("the climb drops one of the close pair, keeps the lone stop, and never loses score", {
  ctx <- build_ctx(five, blocks, trips = 50, riders = 5000)
  for (seed in 1:5) {
    res <- climb(ctx, seed, tight)
    expect_equal(sum(!res$keep[2:3]), 1)
    expect_true(res$keep[4])
    expect_true(is_valid(res$keep, ctx, tight))
    expect_gt(res$score, 0)
    expect_true(all(diff(res$moves$score) > 0))
  }
})

test_that("a fixed stop never moves", {
  fixed_two <- five
  fixed_two$fixed[2] <- TRUE
  ctx <- build_ctx(fixed_two, blocks, trips = 50, riders = 5000)
  res <- climb(ctx, 1, tight)
  expect_true(res$keep[2])
  expect_false(res$keep[3])
})
