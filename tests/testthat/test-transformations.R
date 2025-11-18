# Helper function to check if Julia is set up
skip_if_no_julia <- function() {
  if (!is_setup()) {
    skip("Julia is not set up. Run setup_ForecastBaselines() to run these tests.")
  }
}

test_that("NoTransform can be created", {
  skip_if_no_julia()

  trans <- NoTransform()
  expect_true(!is.null(trans))
})

test_that("LogTransform can be created", {
  skip_if_no_julia()

  trans <- LogTransform()
  expect_true(!is.null(trans))
})

test_that("LogPlusOneTransform can be created", {
  skip_if_no_julia()

  trans <- LogPlusOneTransform()
  expect_true(!is.null(trans))

  trans_custom <- LogPlusOneTransform(c = 0.5)
  expect_true(!is.null(trans_custom))
})

test_that("SquareRootTransform can be created", {
  skip_if_no_julia()

  trans <- SquareRootTransform()
  expect_true(!is.null(trans))
})

test_that("PowerTransform can be created", {
  skip_if_no_julia()

  trans <- PowerTransform(lambda = 0.5)
  expect_true(!is.null(trans))

  trans2 <- PowerTransform(lambda = 0.3)
  expect_true(!is.null(trans2))
})

test_that("PowerPlusOneTransform can be created", {
  skip_if_no_julia()

  trans <- PowerPlusOneTransform(lambda = 0.5)
  expect_true(!is.null(trans))

  trans_custom <- PowerPlusOneTransform(lambda = 0.3, constant = 0.5)
  expect_true(!is.null(trans_custom))
})

test_that("transform_data applies transformation", {
  skip_if_no_julia()

  data <- c(1, 2, 3, 4, 5)
  trans <- LogTransform()

  transformed <- transform_data(data, trans)

  expect_type(transformed, "double")
  expect_length(transformed, length(data))
  expect_true(all(transformed < data))  # log(x) < x for x > 1
})

test_that("inverse_transform_data reverses transformation", {
  skip_if_no_julia()

  data <- c(2, 3, 4, 5, 6)
  trans <- LogTransform()

  transformed <- transform_data(data, trans)
  recovered <- inverse_transform_data(transformed, trans)

  expect_equal(recovered, data, tolerance = 1e-6)
})

test_that("NoTransform is identity transformation", {
  skip_if_no_julia()

  data <- c(1, 2, 3, 4, 5)
  trans <- NoTransform()

  transformed <- transform_data(data, trans)

  expect_equal(transformed, data)
})

test_that("SquareRootTransform works correctly", {
  skip_if_no_julia()

  data <- c(1, 4, 9, 16, 25)
  trans <- SquareRootTransform()

  transformed <- transform_data(data, trans)
  expected <- sqrt(data)

  expect_equal(transformed, expected, tolerance = 1e-6)
})

test_that("LogPlusOneTransform handles zeros", {
  skip_if_no_julia()

  data <- c(0, 1, 2, 3, 4)
  trans <- LogPlusOneTransform()

  transformed <- transform_data(data, trans)

  expect_type(transformed, "double")
  expect_length(transformed, length(data))
  expect_true(all(is.finite(transformed)))
})

test_that("PowerTransform with lambda=0.5 matches square root", {
  skip_if_no_julia()

  data <- c(1, 4, 9, 16, 25)
  trans_power <- PowerTransform(lambda = 0.5)
  trans_sqrt <- SquareRootTransform()

  power_result <- transform_data(data, trans_power)
  sqrt_result <- transform_data(data, trans_sqrt)

  expect_equal(power_result, sqrt_result, tolerance = 1e-6)
})

test_that("transform_model wraps model with transformation", {
  skip_if_no_julia()

  model <- ARMAModel(p = 1)
  trans <- LogTransform()

  transformed_model <- transform_model(model, trans)

  expect_true(!is.null(transformed_model))
})

test_that("Forecasts from transformed model are back-transformed", {
  skip_if_no_julia()

  # Positive data for log transformation
  data <- c(10, 12, 13, 14, 15, 16, 18, 19, 20, 22)

  # Fit model with transformation
  model <- ARMAModel(p = 1)
  trans <- LogTransform()
  transformed_model <- transform_model(model, trans)

  fitted <- fit_baseline(data, transformed_model)
  forecasts <- point_forecast(fitted, horizon = 1:3)

  # Forecasts should be in original scale (positive values similar to data)
  expect_true(all(forecasts > 0))
  expect_true(all(forecasts > min(data) * 0.5))
  expect_true(all(forecasts < max(data) * 2))
})

test_that("PowerPlusOneTransform is invertible", {
  skip_if_no_julia()

  data <- c(0, 1, 2, 3, 4, 5)
  trans <- PowerPlusOneTransform(lambda = 0.3, constant = 1.0)

  transformed <- transform_data(data, trans)
  recovered <- inverse_transform_data(transformed, trans)

  expect_equal(recovered, data, tolerance = 1e-6)
})

test_that("Multiple transformations can be created", {
  skip_if_no_julia()

  trans1 <- LogTransform()
  trans2 <- SquareRootTransform()
  trans3 <- PowerTransform(lambda = 0.5)

  expect_true(!is.null(trans1))
  expect_true(!is.null(trans2))
  expect_true(!is.null(trans3))
})

test_that("Transformations work with model fitting workflow", {
  skip_if_no_julia()

  data <- c(10, 12, 13, 14, 15, 16, 18, 19, 20, 22)

  # Create and fit transformed model
  model <- OLSModel(degree = 1)
  trans <- LogTransform()
  transformed_model <- transform_model(model, trans)

  fitted <- fit_baseline(data, transformed_model)

  # Generate forecast
  fc <- forecast(
    fitted,
    interval_method = NoInterval(),
    horizon = 1:5
  )

  expect_length(fc$mean, 5)
  expect_true(all(is.finite(fc$mean)))
  expect_true(all(fc$mean > 0))
})

test_that("Box-Cox transformation (PowerPlusOneTransform) works", {
  skip_if_no_julia()

  data <- c(10, 12, 13, 14, 15, 16, 18, 19, 20, 22)

  # Box-Cox with lambda = 0.3
  model <- ARMAModel(p = 1)
  trans <- PowerPlusOneTransform(lambda = 0.3)
  transformed_model <- transform_model(model, trans)

  fitted <- fit_baseline(data, transformed_model)
  forecasts <- point_forecast(fitted, horizon = 1:3)

  expect_length(forecasts, 3)
  expect_true(all(is.finite(forecasts)))
})
