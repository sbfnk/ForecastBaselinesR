# Tests for Scoring Rules and Evaluation using scoringutils

# Conversion Tests ------------------------------------------------------

test_that("as_scoringutils_data converts point forecast correctly", {
  skip_if_no_julia()
  skip_on_cran()

  set.seed(123)
  data <- rnorm(50, mean = 100, sd = 10)
  truth_vals <- rnorm(5, mean = 100, sd = 10)

  model <- ConstantModel()
  fitted <- fit_baseline(data, model)

  fc <- forecast(fitted,
    interval_method = NoInterval(),
    horizon = 1:5,
    truth = truth_vals
  )

  fc_data <- as_scoringutils_data(fc, forecast_type = "point")

  expect_s3_class(fc_data, "data.frame")
  expect_true("observed" %in% names(fc_data))
  expect_true("predicted" %in% names(fc_data))
  expect_true("horizon" %in% names(fc_data))
  expect_true("model" %in% names(fc_data))
  expect_equal(nrow(fc_data), 5)
  expect_equal(fc_data$observed, truth_vals)
})

test_that("as_scoringutils_data requires truth values", {
  skip_if_no_julia()
  skip_on_cran()

  set.seed(123)
  data <- rnorm(50, mean = 100, sd = 10)

  model <- ConstantModel()
  fitted <- fit_baseline(data, model)

  # Forecast without truth
  fc <- forecast(fitted,
    interval_method = NoInterval(),
    horizon = 1:5
  )

  expect_error(
    as_scoringutils_data(fc),
    "truth values"
  )
})

# Score Function Tests --------------------------------------------------

test_that("score returns data.frame with summarised scores", {
  skip_if_no_julia()
  skip_on_cran()

  set.seed(123)
  data <- rnorm(50, mean = 100, sd = 10)
  truth_vals <- rnorm(5, mean = 100, sd = 10)

  model <- ConstantModel()
  fitted <- fit_baseline(data, model)

  fc <- forecast(fitted,
    interval_method = NoInterval(),
    horizon = 1:5,
    truth = truth_vals
  )

  scores <- score(fc)

  expect_s3_class(scores, "data.frame")
  expect_true("model" %in% names(scores))
  # Point forecast should have ae_point, se_point, ape
  expect_true(any(c("ae_point", "se_point", "ape") %in% names(scores)))
})

test_that("score returns individual scores when summarise=FALSE", {
  skip_if_no_julia()
  skip_on_cran()

  set.seed(123)
  data <- rnorm(50, mean = 100, sd = 10)
  truth_vals <- rnorm(5, mean = 100, sd = 10)

  model <- ConstantModel()
  fitted <- fit_baseline(data, model)

  fc <- forecast(fitted,
    interval_method = NoInterval(),
    horizon = 1:5,
    truth = truth_vals
  )

  scores <- score(fc, summarise = FALSE)

  expect_s3_class(scores, "data.frame")
  expect_equal(nrow(scores), 5) # One row per horizon
  expect_true("horizon" %in% names(scores))
})

# Metric Convenience Functions ------------------------------------------

test_that("MAE returns numeric value", {
  skip_if_no_julia()
  skip_on_cran()

  set.seed(123)
  data <- rnorm(50, mean = 100, sd = 10)
  truth_vals <- rnorm(5, mean = 100, sd = 10)

  model <- ConstantModel()
  fitted <- fit_baseline(data, model)

  fc <- forecast(fitted,
    interval_method = NoInterval(),
    horizon = 1:5,
    truth = truth_vals
  )

  mae_score <- MAE(fc)

  expect_type(mae_score, "double")
  expect_length(mae_score, 1)
  expect_true(mae_score >= 0)
})

test_that("MSE returns numeric value", {
  skip_if_no_julia()
  skip_on_cran()

  set.seed(123)
  data <- rnorm(50, mean = 100, sd = 10)
  truth_vals <- rnorm(5, mean = 100, sd = 10)

  model <- ConstantModel()
  fitted <- fit_baseline(data, model)

  fc <- forecast(fitted,
    interval_method = NoInterval(),
    horizon = 1:5,
    truth = truth_vals
  )

  mse_score <- MSE(fc)

  expect_type(mse_score, "double")
  expect_length(mse_score, 1)
  expect_true(mse_score >= 0)
})

test_that("RMSE returns numeric value", {
  skip_if_no_julia()
  skip_on_cran()

  set.seed(123)
  data <- rnorm(50, mean = 100, sd = 10)
  truth_vals <- rnorm(5, mean = 100, sd = 10)

  model <- ConstantModel()
  fitted <- fit_baseline(data, model)

  fc <- forecast(fitted,
    interval_method = NoInterval(),
    horizon = 1:5,
    truth = truth_vals
  )

  rmse_score <- RMSE(fc)

  expect_type(rmse_score, "double")
  expect_length(rmse_score, 1)
  expect_true(rmse_score >= 0)
})

test_that("MAPE returns numeric value", {
  skip_if_no_julia()
  skip_on_cran()

  set.seed(123)
  # Use positive values for MAPE
  data <- abs(rnorm(50, mean = 100, sd = 10)) + 10
  truth_vals <- abs(rnorm(5, mean = 100, sd = 10)) + 10

  model <- ConstantModel()
  fitted <- fit_baseline(data, model)

  fc <- forecast(fitted,
    interval_method = NoInterval(),
    horizon = 1:5,
    truth = truth_vals
  )

  mape_score <- MAPE(fc)

  expect_type(mape_score, "double")
  expect_length(mape_score, 1)
  expect_true(mape_score >= 0)
})

# Comparative Tests -----------------------------------------------------

test_that("MAE and RMSE have expected relationship", {
  skip_if_no_julia()
  skip_on_cran()

  set.seed(123)
  data <- rnorm(50, mean = 100, sd = 10)
  truth_vals <- rnorm(5, mean = 100, sd = 10)

  model <- ConstantModel()
  fitted <- fit_baseline(data, model)

  fc <- forecast(fitted,
    interval_method = NoInterval(),
    horizon = 1:5,
    truth = truth_vals
  )

  mae_score <- MAE(fc)
  mse_score <- MSE(fc)
  rmse_score <- RMSE(fc)

  # RMSE should equal sqrt(MSE)
  expect_equal(rmse_score, sqrt(mse_score), tolerance = 1e-10)

  # By Cauchy-Schwarz, RMSE >= MAE (with equality only when all errors are equal)
  expect_true(rmse_score >= mae_score - 1e-10)
})

# get_available_metrics Tests -------------------------------------------

test_that("get_available_metrics returns list for point forecasts", {
  skip_on_cran()

  metrics <- get_available_metrics("point")

  expect_type(metrics, "list")
  expect_true(length(metrics) > 0)
  expect_true("ae_point" %in% names(metrics))
})

test_that("get_available_metrics returns list for quantile forecasts", {
  skip_on_cran()

  metrics <- get_available_metrics("quantile")

  expect_type(metrics, "list")
  expect_true(length(metrics) > 0)
  expect_true("wis" %in% names(metrics))
})

# Deprecated Function Tests ---------------------------------------------

test_that("deprecated functions show warnings", {
  skip_if_no_julia()
  skip_on_cran()

  set.seed(123)
  data <- rnorm(50, mean = 100, sd = 10)
  truth_vals <- rnorm(5, mean = 100, sd = 10)

  model <- ConstantModel()
  fitted <- fit_baseline(data, model)

  fc <- forecast(fitted,
    interval_method = NoInterval(),
    horizon = 1:5,
    truth = truth_vals
  )

  # These functions are deprecated and should throw errors
  expect_error(MdAE(fc), "no longer supported")
  expect_error(MSPE(fc), "no longer supported")
  expect_error(RelativeBias(fc), "no longer supported")
  expect_error(CRPS_trajectory(fc), "no longer supported")
  expect_error(PIT_function(fc), "no longer supported")
  expect_error(CvM_divergence(fc), "no longer supported")
})

# Multiple Models Comparison --------------------------------------------

test_that("score works with multiple horizons and produces correct output", {
  skip_if_no_julia()
  skip_on_cran()

  set.seed(456)
  data <- rnorm(50, mean = 50, sd = 5)
  truth_vals <- rnorm(10, mean = 50, sd = 5)

  model <- ConstantModel()
  fitted <- fit_baseline(data, model)

  fc <- forecast(fitted,
    interval_method = NoInterval(),
    horizon = 1:10,
    truth = truth_vals,
    model_name = "ConstantModel"
  )

  # Get summarised scores
  scores_sum <- score(fc, summarise = TRUE)
  expect_equal(nrow(scores_sum), 1) # One row for the model

  # Get individual scores
  scores_ind <- score(fc, summarise = FALSE)
  expect_equal(nrow(scores_ind), 10) # One row per horizon
})
