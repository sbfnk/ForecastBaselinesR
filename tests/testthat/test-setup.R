test_that("Package setup functions work", {
  # Test is_setup function exists
  expect_true(exists("is_setup"))
  expect_true(exists("setup_ForecastBaselines"))
  expect_true(exists("check_setup"))

  # Test is_setup returns logical
  result <- is_setup()
  expect_type(result, "logical")
})

test_that("Model constructor functions exist", {
  # Test all model constructors are exported
  expect_true(exists("ConstantModel"))
  expect_true(exists("MarginalModel"))
  expect_true(exists("KDEModel"))
  expect_true(exists("LSDModel"))
  expect_true(exists("OLSModel"))
  expect_true(exists("IDSModel"))
  expect_true(exists("STLModel"))
  expect_true(exists("ARMAModel"))
  expect_true(exists("INARCHModel"))
  expect_true(exists("ETSModel"))
})

test_that("Core forecasting functions exist", {
  expect_true(exists("fit_baseline"))
  expect_true(exists("point_forecast"))
  expect_true(exists("forecast"))
  expect_true(exists("interval_forecast"))
})

test_that("Interval method constructors exist", {
  expect_true(exists("NoInterval"))
  expect_true(exists("EmpiricalInterval"))
  expect_true(exists("ParametricInterval"))
  expect_true(exists("ModelTrajectoryInterval"))
})

test_that("Scoring rule constructors exist", {
  expect_true(exists("MAE"))
  expect_true(exists("RMSE"))
  expect_true(exists("CRPS"))
  expect_true(exists("WIS"))
  expect_true(exists("score"))
})

test_that("Utility functions exist", {
  expect_true(exists("add_truth"))
  expect_true(exists("has_mean"))
  expect_true(exists("has_median"))
  expect_true(exists("has_intervals"))
  expect_true(exists("has_truth"))
})

test_that("Transformation functions exist", {
  expect_true(exists("NoTransform"))
  expect_true(exists("LogTransform"))
  expect_true(exists("LogPlusOneTransform"))
  expect_true(exists("SquareRootTransform"))
  expect_true(exists("PowerTransform"))
  expect_true(exists("PowerPlusOneTransform"))
  expect_true(exists("transform_data"))
  expect_true(exists("inverse_transform_data"))
  expect_true(exists("transform_model"))
})
