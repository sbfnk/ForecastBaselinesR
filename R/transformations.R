# Data Transformation Functions

#' No Transformation
#'
#' Creates a transformation that applies no changes to the data.
#'
#' @return A NoTransform object
#' @export
#'
#' @examples
#' \dontrun{
#' trans <- NoTransform()
#' }
NoTransform <- function() {
  check_setup()
  JuliaCall::julia_eval("ForecastBaselines.NoTransform()")
}

#' Log Transformation
#'
#' Creates a natural logarithm transformation. Data must be positive.
#'
#' @return A LogTransform object
#' @export
#'
#' @examples
#' \dontrun{
#' trans <- LogTransform()
#' }
LogTransform <- function() {
  check_setup()
  JuliaCall::julia_eval("ForecastBaselines.LogTransform()")
}

#' Log Plus One Transformation
#'
#' Creates a log(x + c) transformation. Useful for data with zeros.
#'
#' @param c Constant to add before taking log (default: 1)
#'
#' @return A LogPlusOneTransform object
#' @export
#'
#' @examples
#' \dontrun{
#' # Standard log(x + 1)
#' trans <- LogPlusOneTransform()
#'
#' # Custom constant
#' trans <- LogPlusOneTransform(c = 0.5)
#' }
LogPlusOneTransform <- function(c = 1.0) {
  check_setup()
  JuliaCall::julia_call("ForecastBaselines.LogPlusOneTransform", as.numeric(c))
}

#' Square Root Transformation
#'
#' Creates a square root transformation. Useful for count data and variance stabilization.
#'
#' @return A SquareRootTransform object
#' @export
#'
#' @examples
#' \dontrun{
#' trans <- SquareRootTransform()
#' }
SquareRootTransform <- function() {
  check_setup()
  JuliaCall::julia_eval("ForecastBaselines.SquareRootTransform()")
}

#' Power Transformation
#'
#' Creates a power transformation: x^lambda (Box-Cox family).
#'
#' @param lambda Power parameter (lambda = 0 is log, lambda = 0.5 is sqrt)
#'
#' @return A PowerTransform object
#' @export
#'
#' @examples
#' \dontrun{
#' # Square root (equivalent to SquareRootTransform)
#' trans <- PowerTransform(lambda = 0.5)
#'
#' # Cube root
#' trans <- PowerTransform(lambda = 1/3)
#' }
PowerTransform <- function(lambda) {
  check_setup()
  JuliaCall::julia_call("ForecastBaselines.PowerTransform", as.numeric(lambda))
}

#' Power Plus One Transformation
#'
#' Creates a power transformation with a constant: (x + c)^lambda.
#' Useful for Box-Cox transformations with zeros.
#'
#' @param lambda Power parameter
#' @param constant Constant to add before transformation (default: 1.0)
#'
#' @return A PowerPlusOneTransform object
#' @export
#'
#' @examples
#' \dontrun{
#' # Box-Cox transformation
#' trans <- PowerPlusOneTransform(lambda = 0.3)
#'
#' # Custom constant
#' trans <- PowerPlusOneTransform(lambda = 0.5, constant = 0.5)
#' }
PowerPlusOneTransform <- function(lambda, constant = 1.0) {
  check_setup()
  JuliaCall::julia_call("ForecastBaselines.PowerPlusOneTransform",
                       as.numeric(lambda),
                       constant = as.numeric(constant))
}

#' Apply Data Transformation
#'
#' Applies a transformation to data.
#'
#' @param x Numeric vector of data
#' @param transformation A transformation object
#'
#' @return Transformed numeric vector
#' @export
#'
#' @examples
#' \dontrun{
#' data <- c(1, 2, 3, 4, 5)
#' trans <- LogTransform()
#' transformed <- transform_data(data, trans)
#' }
transform_data <- function(x, transformation) {
  check_setup()
  JuliaCall::julia_assign("x_data", as.numeric(x))
  JuliaCall::julia_assign("trans_obj", transformation)
  result <- JuliaCall::julia_eval("ForecastBaselines.transform(x_data, trans_obj)")
  as.numeric(result)
}

#' Apply Inverse Transformation
#'
#' Applies the inverse of a transformation to data (e.g., exp for log).
#'
#' @param y Numeric vector of transformed data
#' @param transformation A transformation object
#'
#' @return Original-scale numeric vector
#' @export
#'
#' @examples
#' \dontrun{
#' transformed <- c(0, 0.693, 1.099, 1.386, 1.609)
#' trans <- LogTransform()
#' original <- inverse_transform_data(transformed, trans)
#' }
inverse_transform_data <- function(y, transformation) {
  check_setup()
  JuliaCall::julia_assign("y_data", as.numeric(y))
  JuliaCall::julia_assign("trans_obj", transformation)
  result <- JuliaCall::julia_eval("ForecastBaselines.inverse_transform(y_data, trans_obj)")
  as.numeric(result)
}

#' Apply Transformation to Model
#'
#' Wraps a model with a data transformation. The model will be fit to
#' transformed data and forecasts will be back-transformed automatically.
#'
#' @param model A model object
#' @param transformation A transformation object
#'
#' @return A transformed model object
#' @export
#'
#' @examples
#' \dontrun{
#' # Fit ARMA model to log-transformed data
#' model <- ARMAModel(p = 1, q = 1)
#' trans <- LogTransform()
#' transformed_model <- transform_model(model, trans)
#' fitted <- fit_baseline(data, transformed_model)
#' }
transform_model <- function(model, transformation) {
  check_setup()
  JuliaCall::julia_assign("model_obj", model)
  JuliaCall::julia_assign("trans_obj", transformation)
  JuliaCall::julia_eval("ForecastBaselines.transform(model_obj, trans_obj)")
}
