# Scoring Rules and Evaluation Functions using scoringutils

#' Convert Forecast to scoringutils Format
#'
#' Converts a ForecastBaselines Forecast object to the format expected by
#' the scoringutils package for scoring.
#'
#' @param forecast A Forecast object (from forecast() function)
#' @param forecast_type Type of forecast: "point" or "quantile". If NULL (default),
#'   will be inferred from the forecast object.
#'
#' @return A data.frame formatted for scoringutils
#' @export
#'
#' @examples
#' \dontrun{
#' # Convert a point forecast
#' fc_data <- as_scoringutils_data(forecast)
#'
#' # Convert a quantile forecast
#' fc_data <- as_scoringutils_data(forecast, forecast_type = "quantile")
#' }
as_scoringutils_data <- function(forecast, forecast_type = NULL) {
  if (!inherits(forecast, "ForecastBaselines_Forecast")) {
    stop("Input must be a ForecastBaselines_Forecast object")
  }

  # Check if truth values are present
  if (is.null(forecast$truth) || all(is.na(forecast$truth))) {
    stop("Forecast must contain truth values for scoring. Use add_truth() to add them.")
  }

  # Determine forecast type if not specified
  if (is.null(forecast_type)) {
    has_quantiles <- !is.null(forecast$quantiles) && length(forecast$quantiles) > 0
    has_intervals <- !is.null(forecast$intervals) && !all(is.na(forecast$intervals))

    if (has_quantiles || has_intervals) {
      forecast_type <- "quantile"
    } else {
      forecast_type <- "point"
    }
  }

  # Get model name
  model_name <- if (!is.null(forecast$model_name)) forecast$model_name else "model"

  if (forecast_type == "point") {
    # Create point forecast format
    # scoringutils expects: observed, predicted, and optional grouping columns
    data.frame(
      observed = forecast$truth,
      predicted = forecast$mean,
      horizon = forecast$horizon,
      model = model_name,
      stringsAsFactors = FALSE
    )
  } else if (forecast_type == "quantile") {
    # For quantile forecasts, we need to expand to long format
    # Each row represents one quantile for one horizon

    # If we have explicit quantiles, use those
    if (!is.null(forecast$quantiles) && length(forecast$quantiles) > 0) {
      # Assume quantiles is a matrix: rows = horizons, cols = quantile levels
      n_horizons <- length(forecast$horizon)
      n_quantiles <- ncol(forecast$quantiles)

      # Create long format
      result <- data.frame(
        observed = rep(forecast$truth, each = n_quantiles),
        predicted = as.vector(t(forecast$quantiles)),
        quantile_level = rep(forecast$quantile_levels, times = n_horizons),
        horizon = rep(forecast$horizon, each = n_quantiles),
        model = model_name,
        stringsAsFactors = FALSE
      )

      return(result)
    }

    # If we have intervals instead, convert them to quantiles
    if (!is.null(forecast$intervals) && !all(is.na(forecast$intervals))) {
      # intervals should contain lower and upper bounds for each level
      # This is more complex - for now, return a warning
      warning("Interval-based forecasts not yet fully supported. Falling back to point forecast.")
      return(as_scoringutils_data(forecast, forecast_type = "point"))
    }

    # No quantile data available
    warning("No quantile or interval data found. Falling back to point forecast.")
    return(as_scoringutils_data(forecast, forecast_type = "point"))
  } else {
    stop("forecast_type must be 'point' or 'quantile'")
  }
}

#' Score a Forecast
#'
#' Evaluates a forecast using scoring metrics from the scoringutils package.
#' The forecast must contain truth values.
#'
#' @param forecast A Forecast object (from forecast() function)
#' @param metrics Optional named list of scoring metrics. If NULL, uses default
#'   metrics from scoringutils based on forecast type.
#' @param forecast_type Type of forecast: "point" or "quantile". If NULL (default),
#'   will be inferred from the forecast object.
#' @param summarise If TRUE (default), returns summarised scores. If FALSE, returns
#'   individual scores for each horizon.
#' @param by Character vector of column names to group by when summarising.
#'   Default is c("model").
#'
#' @return A data.frame with scores. If summarise=TRUE, returns aggregated scores.
#' @export
#'
#' @examples
#' \dontrun{
#' # Score with default metrics
#' scores <- score(forecast)
#'
#' # Score without summarising
#' scores <- score(forecast, summarise = FALSE)
#'
#' # Score with custom metrics
#' custom_metrics <- list(mae = scoringutils::ae_point)
#' scores <- score(forecast, metrics = custom_metrics)
#' }
score <- function(forecast, metrics = NULL, forecast_type = NULL,
                  summarise = TRUE, by = c("model")) {
  # Check if scoringutils is available
  if (!requireNamespace("scoringutils", quietly = TRUE)) {
    stop("Package 'scoringutils' is required but not installed. Install it with: install.packages('scoringutils')")
  }

  # Convert to scoringutils format
  fc_data <- as_scoringutils_data(forecast, forecast_type = forecast_type)

  # Validate forecast data
  if (is.null(forecast_type)) {
    # Infer type from data
    if ("quantile_level" %in% names(fc_data)) {
      validated <- scoringutils::as_forecast_quantile(fc_data)
    } else {
      validated <- scoringutils::as_forecast_point(fc_data)
    }
  } else if (forecast_type == "point") {
    validated <- scoringutils::as_forecast_point(fc_data)
  } else if (forecast_type == "quantile") {
    validated <- scoringutils::as_forecast_quantile(fc_data)
  } else {
    stop("forecast_type must be 'point' or 'quantile'")
  }

  # Score the forecast
  if (is.null(metrics)) {
    scores <- scoringutils::score(validated)
  } else {
    scores <- scoringutils::score(validated, metrics = metrics)
  }

  # Summarise if requested
  if (summarise) {
    scores <- scoringutils::summarise_scores(scores, by = by)
  }

  return(scores)
}

#' Get Available Metrics
#'
#' Returns the default metrics available for a forecast type.
#'
#' @param forecast_type Either "point" or "quantile"
#'
#' @return A named list of scoring functions
#' @export
#'
#' @examples
#' \dontrun{
#' # Get point forecast metrics
#' metrics <- get_available_metrics("point")
#' print(names(metrics))
#'
#' # Get quantile forecast metrics
#' metrics <- get_available_metrics("quantile")
#' print(names(metrics))
#' }
get_available_metrics <- function(forecast_type = "point") {
  if (!requireNamespace("scoringutils", quietly = TRUE)) {
    stop("Package 'scoringutils' is required but not installed.")
  }

  if (forecast_type == "point") {
    # Create a minimal point forecast to get default metrics
    dummy <- data.frame(observed = 1, predicted = 1, model = "dummy")
    validated <- scoringutils::as_forecast_point(dummy)
    return(scoringutils::get_metrics(validated))
  } else if (forecast_type == "quantile") {
    # Create a minimal quantile forecast
    dummy <- data.frame(
      observed = 1, predicted = 1,
      quantile_level = 0.5, model = "dummy"
    )
    validated <- scoringutils::as_forecast_quantile(dummy)
    return(scoringutils::get_metrics(validated))
  } else {
    stop("forecast_type must be 'point' or 'quantile'")
  }
}

# Convenience functions for specific metrics

#' Mean Absolute Error (MAE)
#'
#' Computes the Mean Absolute Error for a forecast. This is a convenience
#' wrapper around score() that returns only the MAE value.
#'
#' @param forecast A Forecast object (from forecast() function)
#'
#' @return Numeric MAE value
#' @export
#'
#' @examples
#' \dontrun{
#' mae_score <- MAE(forecast)
#' }
MAE <- function(forecast) {
  scores <- score(forecast, forecast_type = "point", summarise = TRUE)
  if ("ae_point" %in% names(scores)) {
    return(scores$ae_point[1])
  }
  stop("MAE (ae_point) not available in scores")
}

#' Mean Squared Error (MSE)
#'
#' Computes the Mean Squared Error for a forecast. This is a convenience
#' wrapper around score() that returns only the MSE value.
#'
#' @param forecast A Forecast object (from forecast() function)
#'
#' @return Numeric MSE value
#' @export
#'
#' @examples
#' \dontrun{
#' mse_score <- MSE(forecast)
#' }
MSE <- function(forecast) {
  scores <- score(forecast, forecast_type = "point", summarise = TRUE)
  if ("se_point" %in% names(scores)) {
    return(scores$se_point[1])
  }
  stop("MSE (se_point) not available in scores")
}

#' Root Mean Squared Error (RMSE)
#'
#' Computes the Root Mean Squared Error for a forecast.
#'
#' @param forecast A Forecast object (from forecast() function)
#'
#' @return Numeric RMSE value
#' @export
#'
#' @examples
#' \dontrun{
#' rmse_score <- RMSE(forecast)
#' }
RMSE <- function(forecast) {
  return(sqrt(MSE(forecast)))
}

#' Mean Absolute Percentage Error (MAPE)
#'
#' Computes the Mean Absolute Percentage Error for a forecast. This is a
#' convenience wrapper around score() that returns only the MAPE value.
#'
#' @param forecast A Forecast object (from forecast() function)
#'
#' @return Numeric MAPE value (as a proportion, not percentage)
#' @export
#'
#' @examples
#' \dontrun{
#' mape_score <- MAPE(forecast)
#' }
MAPE <- function(forecast) {
  scores <- score(forecast, forecast_type = "point", summarise = TRUE)
  if ("ape" %in% names(scores)) {
    return(scores$ape[1])
  }
  stop("MAPE (ape) not available in scores")
}

#' Weighted Interval Score (WIS)
#'
#' Computes the Weighted Interval Score for a quantile forecast. This is a
#' proper scoring rule for prediction intervals.
#'
#' @param forecast A Forecast object with quantiles or intervals
#'
#' @return Numeric WIS value
#' @export
#'
#' @examples
#' \dontrun{
#' wis_score <- WIS(forecast)
#' }
WIS <- function(forecast) {
  scores <- score(forecast, forecast_type = "quantile", summarise = TRUE)
  if ("wis" %in% names(scores)) {
    return(scores$wis[1])
  }
  stop("WIS not available in scores. Forecast may not have quantile/interval data.")
}

#' Continuous Ranked Probability Score (CRPS)
#'
#' Note: scoringutils computes CRPS via the WIS for quantile forecasts.
#' This function returns the WIS value, which approximates CRPS.
#'
#' @param forecast A Forecast object with quantiles or intervals
#'
#' @return Numeric CRPS approximation via WIS
#' @export
#'
#' @examples
#' \dontrun{
#' crps_score <- CRPS(forecast)
#' }
CRPS <- function(forecast) {
  # For quantile forecasts, WIS approximates CRPS
  return(WIS(forecast))
}

# Legacy function stubs for compatibility (now deprecated)

#' @export
MdAE <- function(forecast) {
  .Deprecated("score", package = "ForecastBaselinesR",
              msg = "MdAE() is deprecated. Use score() with custom metrics instead.")
  stop("MdAE is no longer supported. Use scoringutils::score() with custom metrics.")
}

#' @export
MSPE <- function(forecast) {
  .Deprecated("score", package = "ForecastBaselinesR",
              msg = "MSPE() is deprecated. Use score() with custom metrics instead.")
  stop("MSPE is no longer supported. Use scoringutils::score() with custom metrics.")
}

#' @export
Bias <- function(forecast) {
  .Deprecated("score", package = "ForecastBaselinesR",
              msg = "Bias() is deprecated. Use score() to get bias from quantile forecasts.")
  scores <- score(forecast, forecast_type = "quantile", summarise = TRUE)
  if ("bias" %in% names(scores)) {
    return(scores$bias[1])
  }
  stop("Bias not available. Requires quantile forecast.")
}

#' @export
RelativeBias <- function(forecast) {
  .Deprecated("score", package = "ForecastBaselinesR",
              msg = "RelativeBias() is deprecated. Use score() with custom metrics instead.")
  stop("RelativeBias is no longer supported. Use scoringutils::score() with custom metrics.")
}

#' @export
CRPS_trajectory <- function(forecast) {
  .Deprecated("score", package = "ForecastBaselinesR",
              msg = "CRPS_trajectory() is deprecated. Use score() instead.")
  stop("CRPS_trajectory is no longer supported. Use scoringutils::score() instead.")
}

#' @export
PIT_function <- function(forecast) {
  .Deprecated(msg = "PIT_function() is deprecated. scoringutils does not provide PIT directly.")
  stop("PIT_function is no longer supported in the scoringutils migration.")
}

#' @export
CvM_divergence <- function(forecast) {
  .Deprecated(msg = "CvM_divergence() is deprecated. scoringutils does not provide CvM directly.")
  stop("CvM_divergence is no longer supported in the scoringutils migration.")
}
