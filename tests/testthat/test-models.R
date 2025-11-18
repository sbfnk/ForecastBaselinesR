test_that("ConstantModel can be created and used", {
  skip_if_no_julia()

  model <- ConstantModel()
  expect_true(!is.null(model))

  fitted <- fit_baseline(sample_data, model)
  expect_true(!is.null(fitted))

  forecasts <- point_forecast(fitted, horizon = 1:3)
  expect_type(forecasts, "double")
  expect_length(forecasts, 3)

  # Constant model should forecast the last value
  expect_equal(forecasts[1], 23, tolerance = 0.01)
})

test_that("MarginalModel can be created and used", {
  skip_if_no_julia()

  model <- MarginalModel(p = 5)
  expect_true(!is.null(model))

  fitted <- fit_baseline(sample_data, model)
  expect_true(!is.null(fitted))

  forecasts <- point_forecast(fitted, horizon = 1:3)
  expect_type(forecasts, "double")
  expect_length(forecasts, 3)

  # All forecasts should be the same (mean of last 5 observations)
  expect_equal(forecasts[1], forecasts[2], tolerance = 0.01)
  expect_equal(forecasts[2], forecasts[3], tolerance = 0.01)
})

test_that("ARMAModel can be created and used", {
  skip_if_no_julia()

  model <- ARMAModel(p = 1, q = 1)
  expect_true(!is.null(model))

  fitted <- fit_baseline(sample_data, model)
  expect_true(!is.null(fitted))

  forecasts <- point_forecast(fitted, horizon = 1:5)
  expect_type(forecasts, "double")
  expect_length(forecasts, 5)
  expect_true(all(!is.na(forecasts)))
  expect_true(all(is.finite(forecasts)))
})

test_that("OLSModel can be created and used", {
  skip_if_no_julia()

  model <- OLSModel(degree = 1)
  expect_true(!is.null(model))

  fitted <- fit_baseline(sample_data, model)
  expect_true(!is.null(fitted))

  forecasts <- point_forecast(fitted, horizon = 1:3)
  expect_type(forecasts, "double")
  expect_length(forecasts, 3)
  expect_true(all(!is.na(forecasts)))
})

test_that("ETSModel can be created with different specifications", {
  skip_if_no_julia()

  # Simple exponential smoothing (A,N,N)
  model <- ETSModel(error_type = "A", trend_type = "N", season_type = "N")
  expect_true(!is.null(model))

  fitted <- fit_baseline(sample_data, model)
  expect_true(!is.null(fitted))

  forecasts <- point_forecast(fitted, horizon = 1:3)
  expect_type(forecasts, "double")
  expect_length(forecasts, 3)
})

test_that("STLModel requires seasonality parameter", {
  skip_if_no_julia()

  # Create seasonal data
  seasonal_data <- sin(2 * pi * (1:48) / 12) * 2 + (1:48) * 0.1 + 10

  model <- STLModel(s = 12)
  expect_true(!is.null(model))

  fitted <- fit_baseline(seasonal_data, model)
  expect_true(!is.null(fitted))

  forecasts <- point_forecast(fitted, horizon = 1:12)
  expect_type(forecasts, "double")
  expect_length(forecasts, 12)
})

test_that("LSDModel works with seasonal data", {
  skip_if_no_julia()

  # Create seasonal data
  seasonal_data <- sin(2 * pi * (1:48) / 12) * 2 + (1:48) * 0.1 + 10

  model <- LSDModel(s = 12, window_width = 1)
  expect_true(!is.null(model))

  fitted <- fit_baseline(seasonal_data, model)
  expect_true(!is.null(fitted))

  forecasts <- point_forecast(fitted, horizon = 1:12)
  expect_type(forecasts, "double")
  expect_length(forecasts, 12)
})

test_that("INARCHModel works with count data", {
  skip_if_no_julia()

  count_data <- c(5, 3, 7, 4, 6, 8, 5, 7, 9, 6, 4, 5, 8, 6, 7)

  model <- INARCHModel(p = 1)
  expect_true(!is.null(model))

  fitted <- fit_baseline(count_data, model)
  expect_true(!is.null(fitted))

  forecasts <- point_forecast(fitted, horizon = 1:3)
  expect_type(forecasts, "double")
  expect_length(forecasts, 3)
  expect_true(all(forecasts >= 0))  # Count forecasts should be non-negative
})

test_that("IDSModel can be created and used", {
  skip_if_no_julia()

  model <- IDSModel(p = 3)
  expect_true(!is.null(model))

  fitted <- fit_baseline(sample_data, model)
  expect_true(!is.null(fitted))

  forecasts <- point_forecast(fitted, horizon = 1:3)
  expect_type(forecasts, "double")
  expect_length(forecasts, 3)
})

test_that("KDEModel can be created and used", {
  skip_if_no_julia()

  model <- KDEModel()
  expect_true(!is.null(model))

  fitted <- fit_baseline(sample_data, model)
  expect_true(!is.null(fitted))

  forecasts <- point_forecast(fitted, horizon = 1:3)
  expect_type(forecasts, "double")
  expect_length(forecasts, 3)
})
