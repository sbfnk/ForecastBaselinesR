# Tests for Scoring Rules and Evaluation

# Point Forecast Scoring Rules ------------------------------------------

test_that("MAE creates valid scoring rule", {
  skip_if_no_julia()

  rule <- MAE()
  expect_true(!is.null(rule))
})

test_that("MdAE creates valid scoring rule", {
  skip_if_no_julia()

  rule <- MdAE()
  expect_true(!is.null(rule))
})

test_that("MAPE creates valid scoring rule", {
  skip_if_no_julia()

  rule <- MAPE()
  expect_true(!is.null(rule))
})

test_that("MSE creates valid scoring rule", {
  skip_if_no_julia()

  rule <- MSE()
  expect_true(!is.null(rule))
})

test_that("MSPE creates valid scoring rule", {
  skip_if_no_julia()

  rule <- MSPE()
  expect_true(!is.null(rule))
})

test_that("RMSE creates valid scoring rule", {
  skip_if_no_julia()

  rule <- RMSE()
  expect_true(!is.null(rule))
})

test_that("Bias creates valid scoring rule", {
  skip_if_no_julia()

  rule <- Bias()
  expect_true(!is.null(rule))
})

test_that("RelativeBias creates valid scoring rule", {
  skip_if_no_julia()

  rule <- RelativeBias()
  expect_true(!is.null(rule))
})

# Probabilistic Scoring Rules -------------------------------------------

test_that("WIS creates valid scoring rule with defaults", {
  skip_if_no_julia()

  rule <- WIS()
  expect_true(!is.null(rule))
})

test_that("WIS creates valid scoring rule with weights", {
  skip_if_no_julia()

  rule <- WIS(weights = c(1, 1, 1, 1, 1))
  expect_true(!is.null(rule))
})

test_that("CRPS creates valid scoring rule", {
  skip_if_no_julia()

  rule <- CRPS()
  expect_true(!is.null(rule))
})

test_that("CRPS_trajectory creates valid scoring rule", {
  skip_if_no_julia()

  rule <- CRPS_trajectory()
  expect_true(!is.null(rule))
})

# score Function Tests --------------------------------------------------

test_that("score evaluates forecast with MAE", {
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
                truth = truth_vals)

  mae_score <- score(fc, MAE())

  expect_type(mae_score, "double")
  expect_length(mae_score, 1)
  expect_true(mae_score >= 0)
})

test_that("score evaluates forecast with MSE", {
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
                truth = truth_vals)

  mse_score <- score(fc, MSE())

  expect_type(mse_score, "double")
  expect_length(mse_score, 1)
  expect_true(mse_score >= 0)
})

test_that("score evaluates forecast with RMSE", {
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
                truth = truth_vals)

  rmse_score <- score(fc, RMSE())

  expect_type(rmse_score, "double")
  expect_length(rmse_score, 1)
  expect_true(rmse_score >= 0)
})

test_that("score evaluates forecast with Bias", {
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
                truth = truth_vals)

  bias_score <- score(fc, Bias())

  expect_type(bias_score, "double")
  expect_length(bias_score, 1)
})

test_that("score evaluates forecast with MdAE", {
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
                truth = truth_vals)

  mdae_score <- score(fc, MdAE())

  expect_type(mdae_score, "double")
  expect_length(mdae_score, 1)
  expect_true(mdae_score >= 0)
})

test_that("score evaluates forecast with MAPE", {
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
                truth = truth_vals)

  mape_score <- score(fc, MAPE())

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
                truth = truth_vals)

  mae_score <- score(fc, MAE())
  mse_score <- score(fc, MSE())
  rmse_score <- score(fc, RMSE())

  # RMSE should equal sqrt(MSE)
  expect_equal(rmse_score, sqrt(mse_score), tolerance = 1e-10)

  # By Cauchy-Schwarz, RMSE >= MAE (with equality only when all errors are equal)
  expect_true(rmse_score >= mae_score - 1e-10)
})
