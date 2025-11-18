# Core Forecasting Functions

#' Fit a Baseline Model
#'
#' Fits a baseline forecasting model to observed data.
#'
#' @param x Numeric vector of time series data
#' @param model A model object created by one of the model constructors
#' @param temporal_info Optional TemporalInfo object with start date and resolution
#'
#' @return A fitted model object that can be used for forecasting
#' @export
#'
#' @examples
#' \dontrun{
#' data <- c(1.2, 2.3, 3.1, 2.8, 3.5, 4.2, 3.9)
#' model <- ARMAModel(p = 1, q = 1)
#' fitted <- fit_baseline(data, model)
#' }
fit_baseline <- function(x, model, temporal_info = NULL) {
  check_setup()

  # Assign data to Julia
  JuliaCall::julia_assign("x_data", as.numeric(x))

  # Assign model to Julia
  JuliaCall::julia_assign("model_obj", model)

  # Fit the model
  if (is.null(temporal_info)) {
    JuliaCall::julia_eval("fitted = ForecastBaselines.fit_baseline(x_data, model_obj)")
  } else {
    JuliaCall::julia_assign("temp_info", temporal_info)
    JuliaCall::julia_eval("fitted = ForecastBaselines.fit_baseline(x_data, model_obj, temporal_info=temp_info)")
  }

  # Return the fitted model
  JuliaCall::julia_eval("fitted")
}

#' Generate Point Forecasts
#'
#' Generates point forecasts from a fitted model.
#'
#' @param fitted A fitted model object from fit_baseline()
#' @param horizon Integer or vector of integers specifying forecast horizons
#'
#' @return Numeric vector of point forecasts
#' @export
#'
#' @examples
#' \dontrun{
#' forecasts <- point_forecast(fitted, horizon = 1:12)
#' }
point_forecast <- function(fitted, horizon = 1L) {
  check_setup()

  JuliaCall::julia_assign("fitted_obj", fitted)

  if (length(horizon) == 1) {
    JuliaCall::julia_assign("h", as.integer(horizon))
  } else {
    JuliaCall::julia_assign("h", as.integer(horizon))
  }

  result <- JuliaCall::julia_eval("ForecastBaselines.point_forecast(fitted_obj, h)")
  as.numeric(result)
}

#' Generate Complete Forecast with Intervals
#'
#' Generates a complete forecast including point forecasts, prediction intervals,
#' and optionally trajectories.
#'
#' @param fitted A fitted model object from fit_baseline()
#' @param interval_method Interval method object (NoInterval, EmpiricalInterval, etc.)
#' @param horizon Integer or vector of integers specifying forecast horizons
#' @param levels Numeric vector of confidence levels (default: 0.95)
#' @param include_median Whether to include median forecast (default: TRUE)
#' @param truth Optional vector of true values for evaluation
#' @param model_name Optional name for the model
#'
#' @return A Forecast object (list) containing forecasts and metadata
#' @export
#'
#' @examples
#' \dontrun{
#' # Point forecast only
#' fc <- forecast(fitted, interval_method = NoInterval(), horizon = 1:12)
#'
#' # With prediction intervals
#' fc <- forecast(fitted,
#'                interval_method = EmpiricalInterval(n_trajectories = 1000),
#'                horizon = 1:12,
#'                levels = c(0.80, 0.95))
#'
#' # With truth for evaluation
#' fc <- forecast(fitted,
#'                interval_method = EmpiricalInterval(),
#'                horizon = 1:12,
#'                truth = c(3.6, 3.8, 4.1, ...))
#' }
forecast <- function(fitted,
                    interval_method = NoInterval(),
                    horizon = 1L,
                    levels = 0.95,
                    include_median = TRUE,
                    truth = NULL,
                    model_name = "") {
  check_setup()

  # Assign objects to Julia
  JuliaCall::julia_assign("fitted_obj", fitted)
  JuliaCall::julia_assign("interval_method_obj", interval_method)
  JuliaCall::julia_assign("h", as.integer(horizon))
  # Ensure levels is always a vector in Julia
  JuliaCall::julia_assign("lvls", as.numeric(levels))
  if (length(levels) == 1) {
    JuliaCall::julia_eval("lvls = [lvls]")
  }
  JuliaCall::julia_assign("inc_median", include_median)
  JuliaCall::julia_assign("mdl_name", as.character(model_name))

  # Build the forecast call
  if (is.null(truth)) {
    forecast_result <- JuliaCall::julia_eval("
      ForecastBaselines.forecast(fitted_obj;
        interval_method = interval_method_obj,
        horizon = h,
        levels = lvls,
        include_median = inc_median,
        model_name = mdl_name)
    ")
  } else {
    JuliaCall::julia_assign("truth_vals", as.numeric(truth))
    forecast_result <- JuliaCall::julia_eval("
      ForecastBaselines.forecast(fitted_obj;
        interval_method = interval_method_obj,
        horizon = h,
        levels = lvls,
        include_median = inc_median,
        truth = truth_vals,
        model_name = mdl_name)
    ")
  }

  # Convert to R-friendly format
  convert_forecast_to_r(forecast_result)
}

#' Generate Interval Forecasts
#'
#' Generates prediction intervals from a fitted model.
#'
#' @param fitted A fitted model object from fit_baseline()
#' @param method Interval method object
#' @param horizon Integer or vector of integers specifying forecast horizons
#' @param levels Numeric vector of confidence levels
#'
#' @return List containing point forecasts, median, intervals, and trajectories
#' @export
#'
#' @examples
#' \dontrun{
#' intervals <- interval_forecast(fitted,
#'                               method = EmpiricalInterval(),
#'                               horizon = 1:12,
#'                               levels = c(0.80, 0.95))
#' }
interval_forecast <- function(fitted, method, horizon = 1L, levels = 0.95) {
  check_setup()

  JuliaCall::julia_assign("fitted_obj", fitted)
  JuliaCall::julia_assign("method_obj", method)
  JuliaCall::julia_assign("h", as.integer(horizon))
  JuliaCall::julia_assign("lvls", as.numeric(levels))

  result <- JuliaCall::julia_eval("
    ForecastBaselines.interval_forecast(fitted_obj, method_obj, h, lvls)
  ")

  # Convert to R list
  list(
    point = as.numeric(result[[1]]),
    median = as.numeric(result[[2]]),
    intervals = result[[3]],
    trajectories = result[[4]]
  )
}

#' Create Temporal Information Object
#'
#' Creates a TemporalInfo object to track dates and time resolution.
#'
#' @param start Start date (Date, POSIXct, or integer)
#' @param resolution Time resolution (integer for generic, or period object)
#'
#' @return A TemporalInfo object
#' @export
#'
#' @examples
#' \dontrun{
#' # Simple integer indexing
#' temp_info <- TemporalInfo(start = 1, resolution = 1)
#'
#' # Date-based
#' temp_info <- TemporalInfo(start = as.Date("2024-01-01"), resolution = 1)
#' }
TemporalInfo <- function(start = 1, resolution = 1) {
  check_setup()

  # Convert R date to Julia if needed
  if (inherits(start, "Date")) {
    start_str <- format(start, "%Y-%m-%d")
    JuliaCall::julia_assign("start_val", start_str)
    JuliaCall::julia_eval("start_date = Date(start_val)")
    JuliaCall::julia_assign("res_val", as.integer(resolution))
    JuliaCall::julia_eval("ForecastBaselines.TemporalInfo(start_date, res_val)")
  } else {
    JuliaCall::julia_call("ForecastBaselines.TemporalInfo",
                         as.integer(start),
                         as.integer(resolution))
  }
}

# Internal helper function to convert Julia Forecast to R list
convert_forecast_to_r <- function(jl_forecast) {
  # Store Julia forecast in a persistent variable for later use (e.g., scoring)
  # Use a unique identifier
  fc_id <- paste0("__fc_", as.integer(Sys.time() * 1000000) %% 1000000)
  JuliaCall::julia_assign(fc_id, jl_forecast)

  # Extract components using Julia field access
  JuliaCall::julia_assign("fc", jl_forecast)

  horizon <- tryCatch({
    as.integer(JuliaCall::julia_eval("fc.horizon"))
  }, error = function(e) NULL)

  mean <- tryCatch({
    as.numeric(JuliaCall::julia_eval("fc.mean"))
  }, error = function(e) NULL)

  median <- tryCatch({
    as.numeric(JuliaCall::julia_eval("fc.median"))
  }, error = function(e) NULL)

  intervals <- tryCatch({
    JuliaCall::julia_eval("fc.intervals")
  }, error = function(e) NULL)

  truth <- tryCatch({
    as.numeric(JuliaCall::julia_eval("fc.truth"))
  }, error = function(e) NULL)

  trajectories <- tryCatch({
    JuliaCall::julia_eval("fc.trajectories")
  }, error = function(e) NULL)

  model_name <- tryCatch({
    as.character(JuliaCall::julia_eval("fc.model_name"))
  }, error = function(e) "")

  # Create R list
  result <- list(
    horizon = horizon,
    mean = mean,
    median = median,
    intervals = intervals,
    truth = truth,
    trajectories = trajectories,
    model_name = model_name,
    .julia_ref = fc_id  # Store Julia reference for scoring
  )

  class(result) <- c("ForecastBaselines_Forecast", "list")
  result
}

#' Print method for Forecast objects
#'
#' @param x A Forecast object
#' @param ... Additional arguments (ignored)
#'
#' @export
print.ForecastBaselines_Forecast <- function(x, ...) {
  cat("ForecastBaselines Forecast Object\n")
  cat("==================================\n\n")

  if (!is.null(x$model_name) && nchar(x$model_name) > 0) {
    cat("Model:", x$model_name, "\n")
  }

  if (!is.null(x$horizon)) {
    cat("Horizon:", min(x$horizon), "to", max(x$horizon), "\n")
  }

  cat("\nComponents:\n")
  if (!is.null(x$mean)) cat("  - Mean forecasts\n")
  if (!is.null(x$median)) cat("  - Median forecasts\n")
  if (!is.null(x$intervals)) cat("  - Prediction intervals\n")
  if (!is.null(x$trajectories)) cat("  - Forecast trajectories\n")
  if (!is.null(x$truth)) cat("  - Truth values\n")

  invisible(x)
}
