# Model Constructors for ForecastBaselines.jl

#' Constant Model
#'
#' Creates a naive forecast model that uses the last observed value as the forecast
#' for all future horizons.
#'
#' @return A ConstantModel object
#' @export
#'
#' @examples
#' \dontrun{
#' model <- ConstantModel()
#' }
ConstantModel <- function() {
  check_setup()
  JuliaCall::julia_eval("ForecastBaselines.ConstantModel()")
}

#' Marginal Model
#'
#' Creates a forecast based on the empirical marginal distribution using the mean
#' of the p most recent observations.
#'
#' @param p Number of most recent observations to use (default: all observations)
#'
#' @return A MarginalModel object
#' @export
#'
#' @examples
#' \dontrun{
#' model <- MarginalModel(p = 10)
#' }
MarginalModel <- function(p = NULL) {
  check_setup()
  if (is.null(p)) {
    JuliaCall::julia_eval("ForecastBaselines.MarginalModel()")
  } else {
    JuliaCall::julia_eval(sprintf(
      "ForecastBaselines.MarginalModel(p=%d)",
      as.integer(p)
    ))
  }
}

#' KDE Model
#'
#' Creates a kernel density estimation model for non-parametric forecasting.
#'
#' @param bandwidth Bandwidth for KDE (default: NULL for automatic selection)
#' @param kernel Kernel function to use (default: "gaussian")
#'
#' @return A KDEModel object
#' @export
#'
#' @examples
#' \dontrun{
#' model <- KDEModel()
#' model <- KDEModel(bandwidth = 0.5)
#' }
KDEModel <- function(bandwidth = NULL, kernel = "gaussian") {
  check_setup()
  if (is.null(bandwidth)) {
    JuliaCall::julia_eval("ForecastBaselines.KDEModel()")
  } else {
    JuliaCall::julia_call("ForecastBaselines.KDEModel", bandwidth)
  }
}

#' Last Similar Dates (LSD) Model
#'
#' Creates a seasonal forecasting model based on similar historical dates.
#'
#' @param s Seasonal period (e.g., 7 for weekly, 12 for monthly)
#' @param window_width Width of the window for averaging similar dates (default: 1)
#' @param trend_correction Whether to apply trend correction (default: FALSE)
#'
#' @return An LSDModel object
#' @export
#'
#' @examples
#' \dontrun{
#' # Weekly seasonality
#' model <- LSDModel(s = 7)
#'
#' # Monthly seasonality with window
#' model <- LSDModel(s = 12, window_width = 2)
#' }
LSDModel <- function(s, window_width = 1L, trend_correction = FALSE) {
  check_setup()
  JuliaCall::julia_eval(sprintf(
    "ForecastBaselines.LSDModel(s=%d, w=%d)",
    as.integer(s),
    as.integer(window_width)
  ))
}

#' OLS Model
#'
#' Creates an ordinary least squares model with polynomial trend.
#'
#' @param degree Polynomial degree (1=linear, 2=quadratic, 3=cubic, etc., default: 1)
#' @param n_obs Number of recent observations to use for fitting (default: NULL, auto-computed as degree + 2)
#'
#' @return An OLSModel object
#' @export
#'
#' @examples
#' \dontrun{
#' # Linear trend (uses last 3 observations by default)
#' model <- OLSModel(degree = 1)
#'
#' # Quadratic trend (uses last 4 observations by default)
#' model <- OLSModel(degree = 2)
#'
#' # Linear trend with specific number of observations
#' model <- OLSModel(degree = 1, n_obs = 10)
#' }
OLSModel <- function(degree = 1L, n_obs = NULL) {
  check_setup()

  # If n_obs not specified, compute as degree + 2 (ensures p >= 1 + d)
  if (is.null(n_obs)) {
    n_obs <- as.integer(degree) + 2L
  }

  # Julia parameters: p (n_obs), d (degree)
  JuliaCall::julia_eval(sprintf(
    "ForecastBaselines.OLSModel(p=%d, d=%d)",
    as.integer(n_obs), as.integer(degree)
  ))
}

#' IDS Model
#'
#' Creates an Increase-Decrease-Stable model for trend detection.
#'
#' @param threshold Threshold for trend detection (default: 0.0)
#' @param window_size Window size for trend calculation (default: 3)
#'
#' @return An IDSModel object
#' @export
#'
#' @examples
#' \dontrun{
#' model <- IDSModel()
#' model <- IDSModel(threshold = 0.1, window_size = 5)
#' }
IDSModel <- function(threshold = 0.0, window_size = 3L) {
  check_setup()
  JuliaCall::julia_eval(sprintf(
    "ForecastBaselines.IDSModel(p=%d)",
    as.integer(window_size)
  ))
}

#' STL Model
#'
#' Creates a Seasonal-Trend decomposition using Loess model.
#'
#' @param s Seasonal period
#' @param trend Whether to include trend component (default: TRUE)
#' @param robust Whether to use robust fitting (default: FALSE)
#'
#' @return An STLModel object
#' @export
#'
#' @examples
#' \dontrun{
#' # Monthly seasonality
#' model <- STLModel(s = 12)
#'
#' # Robust STL
#' model <- STLModel(s = 12, robust = TRUE)
#' }
STLModel <- function(s, trend = TRUE, robust = FALSE) {
  check_setup()
  JuliaCall::julia_eval(sprintf(
    "ForecastBaselines.STLModel(s=%d)",
    as.integer(s)
  ))
}

#' ARMA Model
#'
#' Creates an AutoRegressive Moving Average model.
#'
#' @param p AR order (default: 0)
#' @param q MA order (default: 0)
#' @param s Seasonal period (default: 0 for no seasonality)
#' @param trend Logical indicating whether to include linear trend (default: FALSE)
#'
#' @return An ARMAModel object
#' @export
#'
#' @examples
#' \dontrun{
#' # AR(1)
#' model <- ARMAModel(p = 1)
#'
#' # MA(1)
#' model <- ARMAModel(q = 1)
#'
#' # ARMA(2,1) with seasonality
#' model <- ARMAModel(p = 2, q = 1, s = 12)
#'
#' # ARMA(1,1) with linear trend
#' model <- ARMAModel(p = 1, q = 1, trend = TRUE)
#' }
ARMAModel <- function(p = 0L, q = 0L, s = 0L, trend = FALSE) {
  check_setup()

  # trend should be a boolean value
  JuliaCall::julia_eval(sprintf(
    "ForecastBaselines.ARMAModel(p=%d, q=%d, s=%d, trend=%s)",
    as.integer(p), as.integer(q), as.integer(s),
    tolower(as.character(trend))
  ))
}

#' INARCH Model
#'
#' Creates an Integer-valued ARCH model for count time series.
#'
#' @param p Order of the INARCH model
#'
#' @return An INARCHModel object
#' @export
#'
#' @examples
#' \dontrun{
#' model <- INARCHModel(p = 1)
#' }
INARCHModel <- function(p = 1L) {
  check_setup()
  JuliaCall::julia_eval(sprintf(
    "ForecastBaselines.INARCHModel(p=%d)",
    as.integer(p)
  ))
}

#' ETS Model
#'
#' Creates an Error-Trend-Season exponential smoothing model.
#'
#' @param error_type Error type: "A" (additive), "M" (multiplicative), or "N" (none)
#' @param trend_type Trend type: "A" (additive), "M" (multiplicative), "Ad" (damped additive),
#'                   "Md" (damped multiplicative), or "N" (none)
#' @param season_type Season type: "A" (additive), "M" (multiplicative), or "N" (none)
#' @param s Seasonal period (required if season_type is not "N")
#' @param damped Whether to use damped trend (default: FALSE)
#'
#' @return An ETSModel object
#' @export
#'
#' @examples
#' \dontrun{
#' # Simple exponential smoothing (A,N,N)
#' model <- ETSModel(error_type = "A", trend_type = "N", season_type = "N")
#'
#' # Holt's linear trend (A,A,N)
#' model <- ETSModel(error_type = "A", trend_type = "A", season_type = "N")
#'
#' # Holt-Winters additive (A,A,A)
#' model <- ETSModel(error_type = "A", trend_type = "A", season_type = "A", s = 12)
#'
#' # Holt-Winters multiplicative (M,M,M)
#' model <- ETSModel(error_type = "M", trend_type = "M", season_type = "M", s = 12)
#' }
ETSModel <- function(error_type = "A", trend_type = "N", season_type = "N",
                    s = NULL, damped = FALSE) {
  check_setup()

  # Validate inputs
  valid_error <- c("A", "M", "N")
  valid_trend <- c("A", "M", "Ad", "Md", "N")
  valid_season <- c("A", "M", "N")

  if (!error_type %in% valid_error) {
    stop("error_type must be one of: ", paste(valid_error, collapse = ", "))
  }
  if (!trend_type %in% valid_trend) {
    stop("trend_type must be one of: ", paste(valid_trend, collapse = ", "))
  }
  if (!season_type %in% valid_season) {
    stop("season_type must be one of: ", paste(valid_season, collapse = ", "))
  }

  if (season_type != "N" && is.null(s)) {
    stop("Seasonal period 's' must be provided when season_type is not 'N'")
  }

  # Convert types to Julia symbols
  error_sym <- sprintf(":%s", error_type)
  trend_sym <- sprintf(":%s", trend_type)
  season_sym <- sprintf(":%s", season_type)

  # Call Julia function with keyword arguments
  if (is.null(s)) {
    JuliaCall::julia_eval(sprintf(
      "ForecastBaselines.ETSModel(error=%s, trend=%s, season=%s)",
      error_sym, trend_sym, season_sym
    ))
  } else {
    JuliaCall::julia_eval(sprintf(
      "ForecastBaselines.ETSModel(error=%s, trend=%s, season=%s, s=%d)",
      error_sym, trend_sym, season_sym, as.integer(s)
    ))
  }
}
