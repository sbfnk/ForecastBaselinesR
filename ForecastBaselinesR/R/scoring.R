# Scoring Rules and Evaluation Functions

#' Score a Forecast
#'
#' Evaluates a forecast using a specified scoring rule. The forecast must
#' contain truth values.
#'
#' @param forecast A Forecast object (from forecast() function)
#' @param rule A scoring rule object (MAE, CRPS, WIS, etc.)
#'
#' @return Numeric score value (lower is better for most rules)
#' @export
#'
#' @examples
#' \dontrun{
#' # Score with MAE
#' mae_score <- score(forecast, MAE())
#'
#' # Score with CRPS
#' crps_score <- score(forecast, CRPS())
#' }
score <- function(forecast, rule) {
  check_setup()

  # Use the stored Julia reference if available
  if (inherits(forecast, "ForecastBaselines_Forecast") && !is.null(attr(forecast, "julia_ref"))) {
    fc_var <- attr(forecast, "julia_ref")
    JuliaCall::julia_assign("rule_obj", rule)
    as.numeric(JuliaCall::julia_eval(sprintf("ForecastBaselines.score(%s, rule_obj)", fc_var)))
  } else {
    # Fallback: try to assign as-is (may not work for converted objects)
    JuliaCall::julia_assign("fc", forecast)
    JuliaCall::julia_assign("rule_obj", rule)
    as.numeric(JuliaCall::julia_eval("ForecastBaselines.score(fc, rule_obj)"))
  }
}

# Point Forecast Scoring Rules

#' Mean Absolute Error (MAE)
#'
#' Creates an MAE scoring rule object. MAE measures the average absolute
#' difference between forecasts and observations.
#'
#' @return An MAE scoring rule object
#' @export
#'
#' @examples
#' \dontrun{
#' rule <- MAE()
#' score_value <- score(forecast, rule)
#' }
MAE <- function() {
  check_setup()
  JuliaCall::julia_eval("ForecastBaselines.MAE()")
}

#' Median Absolute Error (MdAE)
#'
#' Creates an MdAE scoring rule object. MdAE measures the median absolute
#' difference between forecasts and observations.
#'
#' @return An MdAE scoring rule object
#' @export
#'
#' @examples
#' \dontrun{
#' rule <- MdAE()
#' score_value <- score(forecast, rule)
#' }
MdAE <- function() {
  check_setup()
  JuliaCall::julia_eval("ForecastBaselines.MdAE()")
}

#' Mean Absolute Percentage Error (MAPE)
#'
#' Creates a MAPE scoring rule object. MAPE measures the average absolute
#' percentage error.
#'
#' @return A MAPE scoring rule object
#' @export
#'
#' @examples
#' \dontrun{
#' rule <- MAPE()
#' score_value <- score(forecast, rule)
#' }
MAPE <- function() {
  check_setup()
  JuliaCall::julia_eval("ForecastBaselines.MAPE()")
}

#' Mean Squared Error (MSE)
#'
#' Creates an MSE scoring rule object. MSE measures the average squared
#' difference between forecasts and observations.
#'
#' @return An MSE scoring rule object
#' @export
#'
#' @examples
#' \dontrun{
#' rule <- MSE()
#' score_value <- score(forecast, rule)
#' }
MSE <- function() {
  check_setup()
  JuliaCall::julia_eval("ForecastBaselines.MSE()")
}

#' Mean Squared Percentage Error (MSPE)
#'
#' Creates an MSPE scoring rule object. MSPE measures the average squared
#' percentage error.
#'
#' @return An MSPE scoring rule object
#' @export
#'
#' @examples
#' \dontrun{
#' rule <- MSPE()
#' score_value <- score(forecast, rule)
#' }
MSPE <- function() {
  check_setup()
  JuliaCall::julia_eval("ForecastBaselines.MSPE()")
}

#' Root Mean Squared Error (RMSE)
#'
#' Creates an RMSE scoring rule object. RMSE is the square root of MSE.
#'
#' @return An RMSE scoring rule object
#' @export
#'
#' @examples
#' \dontrun{
#' rule <- RMSE()
#' score_value <- score(forecast, rule)
#' }
RMSE <- function() {
  check_setup()
  JuliaCall::julia_eval("ForecastBaselines.RMSE()")
}

#' Bias
#'
#' Creates a Bias scoring rule object. Bias measures the average difference
#' between forecasts and observations (can be positive or negative).
#'
#' @return A Bias scoring rule object
#' @export
#'
#' @examples
#' \dontrun{
#' rule <- Bias()
#' score_value <- score(forecast, rule)
#' }
Bias <- function() {
  check_setup()
  JuliaCall::julia_eval("ForecastBaselines.Bias()")
}

#' Relative Bias
#'
#' Creates a RelativeBias scoring rule object. RelativeBias measures bias
#' relative to the magnitude of observations.
#'
#' @return A RelativeBias scoring rule object
#' @export
#'
#' @examples
#' \dontrun{
#' rule <- RelativeBias()
#' score_value <- score(forecast, rule)
#' }
RelativeBias <- function() {
  check_setup()
  JuliaCall::julia_eval("ForecastBaselines.RelativeBias()")
}

# Probabilistic Scoring Rules

#' Weighted Interval Score (WIS)
#'
#' Creates a WIS scoring rule object. WIS is a proper scoring rule for
#' prediction intervals that combines sharpness and calibration.
#'
#' @param weights Optional vector of weights for different horizons
#'
#' @return A WIS scoring rule object
#' @export
#'
#' @examples
#' \dontrun{
#' rule <- WIS()
#' score_value <- score(forecast, rule)
#' }
WIS <- function(weights = NULL) {
  check_setup()
  if (is.null(weights)) {
    JuliaCall::julia_eval("ForecastBaselines.WIS()")
  } else {
    # WIS expects keyword arguments, using level_weights
    JuliaCall::julia_assign("wts", as.numeric(weights))
    JuliaCall::julia_eval("ForecastBaselines.WIS(level_weights=wts)")
  }
}

#' Continuous Ranked Probability Score (CRPS)
#'
#' Creates a CRPS scoring rule object. CRPS is a proper scoring rule for
#' probabilistic forecasts that generalizes MAE to distributions.
#'
#' @return A CRPS scoring rule object
#' @export
#'
#' @examples
#' \dontrun{
#' rule <- CRPS()
#' score_value <- score(forecast, rule)
#' }
CRPS <- function() {
  check_setup()
  JuliaCall::julia_eval("ForecastBaselines.CRPS()")
}

#' CRPS from Trajectories
#'
#' Creates a CRPS_trajectory scoring rule object. Computes CRPS from
#' forecast trajectories/samples.
#'
#' @return A CRPS_trajectory scoring rule object
#' @export
#'
#' @examples
#' \dontrun{
#' rule <- CRPS_trajectory()
#' score_value <- score(forecast, rule)
#' }
CRPS_trajectory <- function() {
  check_setup()
  JuliaCall::julia_eval("ForecastBaselines.CRPS_trajectory()")
}

# Calibration Assessment

#' Probability Integral Transform (PIT)
#'
#' Computes the PIT function for calibration assessment. The PIT values should
#' be uniformly distributed if the forecast is well-calibrated.
#'
#' @param forecast A Forecast object with prediction intervals and truth values
#'
#' @return Numeric vector of PIT values
#' @export
#'
#' @examples
#' \dontrun{
#' pit_values <- PIT_function(forecast)
#' hist(pit_values) # Should be roughly uniform
#' }
PIT_function <- function(forecast) {
  check_setup()

  # Use the stored Julia reference if available
  if (inherits(forecast, "ForecastBaselines_Forecast") && !is.null(attr(forecast, "julia_ref"))) {
    fc_var <- attr(forecast, "julia_ref")
    as.numeric(JuliaCall::julia_eval(sprintf("ForecastBaselines.PIT_function(%s)", fc_var)))
  } else {
    # Fallback
    JuliaCall::julia_assign("fc", forecast)
    as.numeric(JuliaCall::julia_eval("ForecastBaselines.PIT_function(fc)"))
  }
}

#' Cramér-von Mises Divergence
#'
#' Computes the Cramér-von Mises (CvM) divergence to assess calibration.
#' Measures how far the PIT distribution is from uniform.
#'
#' @param forecast A Forecast object with prediction intervals and truth values
#'
#' @return Numeric CvM divergence value (0 = perfect calibration)
#' @export
#'
#' @examples
#' \dontrun{
#' cvm <- CvM_divergence(forecast)
#' cat("CvM divergence:", cvm, "\n")
#' }
CvM_divergence <- function(forecast) {
  check_setup()

  # Use the stored Julia reference if available
  if (inherits(forecast, "ForecastBaselines_Forecast") && !is.null(attr(forecast, "julia_ref"))) {
    fc_var <- attr(forecast, "julia_ref")
    as.numeric(JuliaCall::julia_eval(sprintf("ForecastBaselines.CvM_divergence(%s)", fc_var)))
  } else {
    # Fallback
    JuliaCall::julia_assign("fc", forecast)
    as.numeric(JuliaCall::julia_eval("ForecastBaselines.CvM_divergence(fc)"))
  }
}
