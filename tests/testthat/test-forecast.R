# Helper function to check if Julia is set up
skip_if_no_julia <- function() {
  if (!is_setup()) {
    skip("Julia is not set up. Run setup_ForecastBaselines() to run these tests.")
  }
}

# Sample data for testing
sample_data <- c(10, 12, 13, 14, 15, 16, 18, 19, 20, 22, 21, 23)

test_that("forecast with NoInterval returns point forecasts only", {
  skip_if_no_julia()

  model <- ConstantModel()
  fitted <- fit_baseline(sample_data, model)

  fc <- forecast(
    fitted,
    interval_method = NoInterval(),
    horizon = 1:3
  )

  expect_true(inherits(fc, "ForecastBaselines_Forecast"))
  expect_true(!is.null(fc$mean))
  expect_length(fc$mean, 3)
  expect_null(fc$intervals)
})

test_that("forecast with EmpiricalInterval returns intervals", {
  skip_if_no_julia()

  model <- ARMAModel(p = 1)
  fitted <- fit_baseline(sample_data, model)

  fc <- forecast(
    fitted,
    interval_method = EmpiricalInterval(n_trajectories = 100, seed = 42),
    horizon = 1:3,
    levels = c(0.80, 0.95)
  )

  expect_true(inherits(fc, "ForecastBaselines_Forecast"))
  expect_true(!is.null(fc$mean))
  expect_length(fc$mean, 3)
  expect_true(!is.null(fc$intervals))
})

test_that("forecast can include truth values", {
  skip_if_no_julia()

  model <- ConstantModel()
  fitted <- fit_baseline(sample_data, model)
  truth <- c(24, 25, 26)

  fc <- forecast(
    fitted,
    interval_method = NoInterval(),
    horizon = 1:3,
    truth = truth
  )

  expect_true(!is.null(fc$truth))
  expect_equal(fc$truth, truth)
})

test_that("forecast with model_name stores name correctly", {
  skip_if_no_julia()

  model <- ARMAModel(p = 1)
  fitted <- fit_baseline(sample_data, model)

  fc <- forecast(
    fitted,
    interval_method = NoInterval(),
    horizon = 1:3,
    model_name = "Test Model"
  )

  expect_equal(fc$model_name, "Test Model")
})

test_that("forecast with multiple horizons works", {
  skip_if_no_julia()

  model <- OLSModel(degree = 1)
  fitted <- fit_baseline(sample_data, model)

  fc <- forecast(
    fitted,
    interval_method = NoInterval(),
    horizon = c(1, 3, 5, 10)
  )

  expect_length(fc$mean, 4)
  expect_length(fc$horizon, 4)
})

test_that("forecast with multiple confidence levels works", {
  skip_if_no_julia()

  model <- ARMAModel(p = 1)
  fitted <- fit_baseline(sample_data, model)

  fc <- forecast(
    fitted,
    interval_method = EmpiricalInterval(n_trajectories = 100),
    horizon = 1:3,
    levels = c(0.50, 0.80, 0.95)
  )

  expect_true(!is.null(fc$intervals))
})

test_that("ParametricInterval method works", {
  skip_if_no_julia()

  model <- ARMAModel(p = 1, q = 1)
  fitted <- fit_baseline(sample_data, model)

  fc <- forecast(
    fitted,
    interval_method = ParametricInterval(),
    horizon = 1:3,
    levels = 0.95
  )

  expect_true(!is.null(fc$intervals))
  expect_length(fc$mean, 3)
})

test_that("ModelTrajectoryInterval method works", {
  skip_if_no_julia()

  model <- ARMAModel(p = 1)
  fitted <- fit_baseline(sample_data, model)

  fc <- forecast(
    fitted,
    interval_method = ModelTrajectoryInterval(n_trajectories = 100, seed = 123),
    horizon = 1:3,
    levels = 0.95
  )

  expect_true(!is.null(fc$intervals))
  expect_length(fc$mean, 3)
})

test_that("forecast object prints correctly", {
  skip_if_no_julia()

  model <- ConstantModel()
  fitted <- fit_baseline(sample_data, model)

  fc <- forecast(
    fitted,
    interval_method = NoInterval(),
    horizon = 1:5,
    model_name = "Constant"
  )

  # Test that print method works without error
  expect_output(print(fc), "ForecastBaselines Forecast Object")
})

test_that("point_forecast returns correct length", {
  skip_if_no_julia()

  model <- ARMAModel(p = 1)
  fitted <- fit_baseline(sample_data, model)

  # Single horizon
  fc1 <- point_forecast(fitted, horizon = 5)
  expect_length(fc1, 1)

  # Multiple horizons
  fc2 <- point_forecast(fitted, horizon = 1:10)
  expect_length(fc2, 10)

  # Non-sequential horizons
  fc3 <- point_forecast(fitted, horizon = c(1, 3, 5))
  expect_length(fc3, 3)
})

test_that("interval_forecast returns expected structure", {
  skip_if_no_julia()

  model <- ARMAModel(p = 1)
  fitted <- fit_baseline(sample_data, model)

  result <- interval_forecast(
    fitted,
    method = EmpiricalInterval(n_trajectories = 50),
    horizon = 1:3,
    levels = 0.95
  )

  expect_type(result, "list")
  expect_true("point" %in% names(result))
  expect_true("median" %in% names(result))
  expect_true("intervals" %in% names(result))
  expect_true("trajectories" %in% names(result))
})

test_that("forecast with include_median=FALSE works", {
  skip_if_no_julia()

  model <- ARMAModel(p = 1)
  fitted <- fit_baseline(sample_data, model)

  fc <- forecast(
    fitted,
    interval_method = EmpiricalInterval(n_trajectories = 50),
    horizon = 1:3,
    include_median = FALSE
  )

  expect_true(!is.null(fc$mean))
})

test_that("EmpiricalInterval with return_trajectories works", {
  skip_if_no_julia()

  model <- ARMAModel(p = 1)
  fitted <- fit_baseline(sample_data, model)

  fc <- forecast(
    fitted,
    interval_method = EmpiricalInterval(
      n_trajectories = 50,
      return_trajectories = TRUE
    ),
    horizon = 1:3
  )

  expect_true(!is.null(fc$trajectories))
})

test_that("EmpiricalInterval with positivity_correction works", {
  skip_if_no_julia()

  model <- ARMAModel(p = 1)
  fitted <- fit_baseline(sample_data, model)

  fc <- forecast(
    fitted,
    interval_method = EmpiricalInterval(
      n_trajectories = 50,
      positivity_correction = "post_clip"
    ),
    horizon = 1:3
  )

  expect_true(!is.null(fc$mean))
  expect_true(all(fc$mean >= 0))
})
