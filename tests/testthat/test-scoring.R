test_that("MAE scoring rule works", {
  skip_if_no_julia()

  model <- ConstantModel()
  fitted <- fit_baseline(sample_data, model)
  fc <- forecast(
    fitted,
    interval_method = NoInterval(),
    horizon = 1:3,
    truth = truth_values
  )

  mae_score <- score(fc, MAE())
  expect_type(mae_score, "double")
  expect_length(mae_score, 1)
  expect_true(mae_score >= 0)
  expect_true(is.finite(mae_score))
})

test_that("RMSE scoring rule works", {
  skip_if_no_julia()

  model <- ConstantModel()
  fitted <- fit_baseline(sample_data, model)
  fc <- forecast(
    fitted,
    interval_method = NoInterval(),
    horizon = 1:3,
    truth = truth_values
  )

  rmse_score <- score(fc, RMSE())
  expect_type(rmse_score, "double")
  expect_length(rmse_score, 1)
  expect_true(rmse_score >= 0)
  expect_true(is.finite(rmse_score))
})

test_that("MSE scoring rule works", {
  skip_if_no_julia()

  model <- ConstantModel()
  fitted <- fit_baseline(sample_data, model)
  fc <- forecast(
    fitted,
    interval_method = NoInterval(),
    horizon = 1:3,
    truth = truth_values
  )

  mse_score <- score(fc, MSE())
  expect_type(mse_score, "double")
  expect_length(mse_score, 1)
  expect_true(mse_score >= 0)
})

test_that("Bias scoring rule works", {
  skip_if_no_julia()

  model <- ConstantModel()
  fitted <- fit_baseline(sample_data, model)
  fc <- forecast(
    fitted,
    interval_method = NoInterval(),
    horizon = 1:3,
    truth = truth_values
  )

  bias_score <- score(fc, Bias())
  expect_type(bias_score, "double")
  expect_length(bias_score, 1)
  expect_true(is.finite(bias_score))
})

test_that("MAPE scoring rule works", {
  skip_if_no_julia()

  model <- ConstantModel()
  fitted <- fit_baseline(sample_data, model)
  fc <- forecast(
    fitted,
    interval_method = NoInterval(),
    horizon = 1:3,
    truth = truth_values
  )

  mape_score <- score(fc, MAPE())
  expect_type(mape_score, "double")
  expect_length(mape_score, 1)
  expect_true(mape_score >= 0)
})

test_that("MdAE scoring rule works", {
  skip_if_no_julia()

  model <- ConstantModel()
  fitted <- fit_baseline(sample_data, model)
  fc <- forecast(
    fitted,
    interval_method = NoInterval(),
    horizon = 1:3,
    truth = truth_values
  )

  mdae_score <- score(fc, MdAE())
  expect_type(mdae_score, "double")
  expect_length(mdae_score, 1)
  expect_true(mdae_score >= 0)
})

test_that("MSPE scoring rule works", {
  skip_if_no_julia()

  model <- ConstantModel()
  fitted <- fit_baseline(sample_data, model)
  fc <- forecast(
    fitted,
    interval_method = NoInterval(),
    horizon = 1:3,
    truth = truth_values
  )

  mspe_score <- score(fc, MSPE())
  expect_type(mspe_score, "double")
  expect_length(mspe_score, 1)
  expect_true(mspe_score >= 0)
})

test_that("RelativeBias scoring rule works", {
  skip_if_no_julia()

  model <- ConstantModel()
  fitted <- fit_baseline(sample_data, model)
  fc <- forecast(
    fitted,
    interval_method = NoInterval(),
    horizon = 1:3,
    truth = truth_values
  )

  rb_score <- score(fc, RelativeBias())
  expect_type(rb_score, "double")
  expect_length(rb_score, 1)
  expect_true(is.finite(rb_score))
})

test_that("CRPS scoring rule works with intervals", {
  skip_if_no_julia()

  model <- ARMAModel(p = 1)
  fitted <- fit_baseline(sample_data, model)
  fc <- forecast(
    fitted,
    interval_method = EmpiricalInterval(n_trajectories = 100),
    horizon = 1:3,
    levels = 0.95,
    truth = truth_values
  )

  crps_score <- score(fc, CRPS())
  expect_type(crps_score, "double")
  expect_length(crps_score, 1)
  expect_true(crps_score >= 0)
  expect_true(is.finite(crps_score))
})

test_that("WIS scoring rule works with intervals", {
  skip_if_no_julia()

  model <- ARMAModel(p = 1)
  fitted <- fit_baseline(sample_data, model)
  fc <- forecast(
    fitted,
    interval_method = EmpiricalInterval(n_trajectories = 100),
    horizon = 1:3,
    levels = c(0.80, 0.95),
    truth = truth_values
  )

  wis_score <- score(fc, WIS())
  expect_type(wis_score, "double")
  expect_length(wis_score, 1)
  expect_true(wis_score >= 0)
  expect_true(is.finite(wis_score))
})

test_that("CRPS_trajectory works with trajectories", {
  skip_if_no_julia()

  model <- ARMAModel(p = 1)
  fitted <- fit_baseline(sample_data, model)
  fc <- forecast(
    fitted,
    interval_method = EmpiricalInterval(
      n_trajectories = 100,
      return_trajectories = TRUE
    ),
    horizon = 1:3,
    truth = truth_values
  )

  crps_score <- score(fc, CRPS_trajectory())
  expect_type(crps_score, "double")
  expect_length(crps_score, 1)
  expect_true(crps_score >= 0)
})

test_that("Multiple scoring rules can be applied to same forecast", {
  skip_if_no_julia()

  model <- ARMAModel(p = 1)
  fitted <- fit_baseline(sample_data, model)
  fc <- forecast(
    fitted,
    interval_method = EmpiricalInterval(n_trajectories = 100),
    horizon = 1:3,
    levels = 0.95,
    truth = truth_values
  )

  mae_score <- score(fc, MAE())
  rmse_score <- score(fc, RMSE())
  crps_score <- score(fc, CRPS())

  expect_true(all(is.finite(c(mae_score, rmse_score, crps_score))))
  expect_true(all(c(mae_score, rmse_score, crps_score) >= 0))
})

test_that("Scoring rules require truth values", {
  skip_if_no_julia()

  model <- ConstantModel()
  fitted <- fit_baseline(sample_data, model)
  fc <- forecast(
    fitted,
    interval_method = NoInterval(),
    horizon = 1:3
    # No truth provided
  )

  # Scoring without truth should either error or return missing/NaN
  # Try to score and check result
  result <- tryCatch(
    score(fc, MAE()),
    error = function(e) NA_real_
  )

  # Either errored (result is NA) or returned a non-finite value
  expect_true(is.na(result) || !is.finite(result))
})

test_that("PIT_function works with intervals and truth", {
  skip_if_no_julia()

  model <- ARMAModel(p = 1)
  fitted <- fit_baseline(sample_data, model)
  fc <- forecast(
    fitted,
    interval_method = EmpiricalInterval(n_trajectories = 200),
    horizon = 1:3,
    levels = seq(0.1, 0.9, by = 0.1),
    truth = truth_values
  )

  pit_values <- PIT_function(fc)
  expect_type(pit_values, "double")
  expect_true(all(pit_values >= 0))
  expect_true(all(pit_values <= 1))
})

test_that("CvM_divergence works with intervals and truth", {
  skip_if_no_julia()

  model <- ARMAModel(p = 1)
  fitted <- fit_baseline(sample_data, model)
  fc <- forecast(
    fitted,
    interval_method = EmpiricalInterval(n_trajectories = 200),
    horizon = 1:3,
    levels = seq(0.1, 0.9, by = 0.1),
    truth = truth_values
  )

  cvm_div <- CvM_divergence(fc)
  expect_type(cvm_div, "double")
  expect_length(cvm_div, 1)
  expect_true(cvm_div >= 0)
})
