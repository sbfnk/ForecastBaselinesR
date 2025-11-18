test_that("add_truth adds truth to forecast", {
  skip_if_no_julia()

  model <- ConstantModel()
  fitted <- fit_baseline(sample_data, model)
  fc <- forecast(
    fitted,
    interval_method = NoInterval(),
    horizon = 1:3
  )

  truth <- c(24, 25, 26)
  fc_with_truth <- add_truth(fc, truth)

  expect_true(!is.null(fc_with_truth$truth))
  expect_equal(fc_with_truth$truth, truth)
})

test_that("has_mean detects mean forecasts", {
  skip_if_no_julia()

  model <- ConstantModel()
  fitted <- fit_baseline(sample_data, model)
  fc <- forecast(
    fitted,
    interval_method = NoInterval(),
    horizon = 1:3
  )

  expect_true(has_mean(fc))
})

test_that("has_median detects median forecasts", {
  skip_if_no_julia()

  model <- ARMAModel(p = 1)
  fitted <- fit_baseline(sample_data, model)
  fc <- forecast(
    fitted,
    interval_method = EmpiricalInterval(n_trajectories = 50),
    horizon = 1:3,
    include_median = TRUE
  )

  expect_true(has_median(fc))
})

test_that("has_intervals detects intervals", {
  skip_if_no_julia()

  model <- ARMAModel(p = 1)
  fitted <- fit_baseline(sample_data, model)

  fc_no_int <- forecast(
    fitted,
    interval_method = NoInterval(),
    horizon = 1:3
  )
  expect_false(has_intervals(fc_no_int))

  fc_with_int <- forecast(
    fitted,
    interval_method = EmpiricalInterval(n_trajectories = 50),
    horizon = 1:3,
    levels = 0.95
  )
  expect_true(has_intervals(fc_with_int))
})

test_that("has_truth detects truth values", {
  skip_if_no_julia()

  model <- ConstantModel()
  fitted <- fit_baseline(sample_data, model)

  fc_no_truth <- forecast(
    fitted,
    interval_method = NoInterval(),
    horizon = 1:3
  )
  expect_false(has_truth(fc_no_truth))

  fc_with_truth <- forecast(
    fitted,
    interval_method = NoInterval(),
    horizon = 1:3,
    truth = c(24, 25, 26)
  )
  expect_true(has_truth(fc_with_truth))
})

test_that("has_trajectories detects trajectories", {
  skip_if_no_julia()

  model <- ARMAModel(p = 1)
  fitted <- fit_baseline(sample_data, model)

  fc_no_traj <- forecast(
    fitted,
    interval_method = EmpiricalInterval(
      n_trajectories = 50,
      return_trajectories = FALSE
    ),
    horizon = 1:3
  )
  expect_false(has_trajectories(fc_no_traj))

  fc_with_traj <- forecast(
    fitted,
    interval_method = EmpiricalInterval(
      n_trajectories = 50,
      return_trajectories = TRUE
    ),
    horizon = 1:3
  )
  expect_true(has_trajectories(fc_with_traj))
})

test_that("has_horizon detects horizon", {
  skip_if_no_julia()

  model <- ConstantModel()
  fitted <- fit_baseline(sample_data, model)
  fc <- forecast(
    fitted,
    interval_method = NoInterval(),
    horizon = 1:3
  )

  expect_true(has_horizon(fc))
})

test_that("truncate_horizon reduces forecast length", {
  skip_if_no_julia()

  model <- ConstantModel()
  fitted <- fit_baseline(sample_data, model)
  fc <- forecast(
    fitted,
    interval_method = NoInterval(),
    horizon = 1:10
  )

  fc_truncated <- truncate_horizon(fc, 5)

  expect_true(length(fc_truncated$mean) <= 5)
  expect_true(length(fc_truncated$horizon) <= 5)
})

test_that("filter_horizons keeps only specified horizons", {
  skip_if_no_julia()

  model <- ConstantModel()
  fitted <- fit_baseline(sample_data, model)
  fc <- forecast(
    fitted,
    interval_method = NoInterval(),
    horizon = 1:10
  )

  fc_filtered <- filter_horizons(fc, c(1, 3, 5))

  expect_length(fc_filtered$horizon, 3)
  expect_equal(fc_filtered$horizon, c(1, 3, 5))
})

test_that("filter_levels keeps only specified confidence levels", {
  skip_if_no_julia()

  model <- ARMAModel(p = 1)
  fitted <- fit_baseline(sample_data, model)
  fc <- forecast(
    fitted,
    interval_method = EmpiricalInterval(n_trajectories = 50),
    horizon = 1:3,
    levels = c(0.50, 0.80, 0.95)
  )

  fc_filtered <- filter_levels(fc, c(0.80, 0.95))

  expect_true(!is.null(fc_filtered$intervals))
})

test_that("add_median adds median to forecast", {
  skip_if_no_julia()

  model <- ConstantModel()
  fitted <- fit_baseline(sample_data, model)
  fc <- forecast(
    fitted,
    interval_method = NoInterval(),
    horizon = 1:3
  )

  median_vals <- c(23.5, 23.5, 23.5)
  fc_with_median <- add_median(fc, median_vals)

  expect_true(!is.null(fc_with_median$median))
  expect_equal(fc_with_median$median, median_vals)
})

test_that("add_trajectories adds trajectories to forecast", {
  skip_if_no_julia()

  model <- ConstantModel()
  fitted <- fit_baseline(sample_data, model)
  fc <- forecast(
    fitted,
    interval_method = NoInterval(),
    horizon = 1:3
  )

  trajectories <- matrix(rnorm(30), nrow = 10, ncol = 3)
  fc_with_traj <- add_trajectories(fc, trajectories)

  expect_true(!is.null(fc_with_traj$trajectories))
})

test_that("Forecast object class is correct", {
  skip_if_no_julia()

  model <- ConstantModel()
  fitted <- fit_baseline(sample_data, model)
  fc <- forecast(
    fitted,
    interval_method = NoInterval(),
    horizon = 1:3
  )

  expect_true(inherits(fc, "ForecastBaselines_Forecast"))
  expect_true(inherits(fc, "list"))
})
