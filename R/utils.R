# Forecast Utility Functions

# Query Functions

#' Check if Forecast has Horizon
#'
#' @param forecast A Forecast object
#' @return Logical TRUE/FALSE
#' @export
has_horizon <- function(forecast) {
  check_setup()
  JuliaCall::julia_assign("fc", forecast)
  as.logical(JuliaCall::julia_eval("ForecastBaselines.has_horizon(fc)"))
}

#' Check if Forecast has Mean
#'
#' @param forecast A Forecast object
#' @return Logical TRUE/FALSE
#' @export
has_mean <- function(forecast) {
  check_setup()
  JuliaCall::julia_assign("fc", forecast)
  as.logical(JuliaCall::julia_eval("ForecastBaselines.has_mean(fc)"))
}

#' Check if Forecast has Median
#'
#' @param forecast A Forecast object
#' @return Logical TRUE/FALSE
#' @export
has_median <- function(forecast) {
  check_setup()
  JuliaCall::julia_assign("fc", forecast)
  as.logical(JuliaCall::julia_eval("ForecastBaselines.has_median(fc)"))
}

#' Check if Forecast has Intervals
#'
#' @param forecast A Forecast object
#' @return Logical TRUE/FALSE
#' @export
has_intervals <- function(forecast) {
  check_setup()
  JuliaCall::julia_assign("fc", forecast)
  as.logical(JuliaCall::julia_eval("ForecastBaselines.has_intervals(fc)"))
}

#' Check if Forecast has Truth Values
#'
#' @param forecast A Forecast object
#' @return Logical TRUE/FALSE
#' @export
has_truth <- function(forecast) {
  check_setup()
  JuliaCall::julia_assign("fc", forecast)
  as.logical(JuliaCall::julia_eval("ForecastBaselines.has_truth(fc)"))
}

#' Check if Forecast has Trajectories
#'
#' @param forecast A Forecast object
#' @return Logical TRUE/FALSE
#' @export
has_trajectories <- function(forecast) {
  check_setup()
  JuliaCall::julia_assign("fc", forecast)
  as.logical(JuliaCall::julia_eval("ForecastBaselines.has_trajectories(fc)"))
}

# Modification Functions

#' Add Truth Values to Forecast
#'
#' @param forecast A Forecast object
#' @param truth Numeric vector of observed values
#' @return Updated Forecast object
#' @export
add_truth <- function(forecast, truth) {
  check_setup()
  JuliaCall::julia_assign("fc", forecast)
  JuliaCall::julia_assign("truth_vals", as.numeric(truth))
  result <- JuliaCall::julia_eval("ForecastBaselines.add_truth(fc, truth_vals)")
  convert_forecast_to_r(result)
}

#' Add Median to Forecast
#'
#' @param forecast A Forecast object
#' @param median Numeric vector of median forecasts
#' @return Updated Forecast object
#' @export
add_median <- function(forecast, median) {
  check_setup()
  JuliaCall::julia_assign("fc", forecast)
  JuliaCall::julia_assign("med_vals", as.numeric(median))
  result <- JuliaCall::julia_eval("ForecastBaselines.add_median(fc, med_vals)")
  convert_forecast_to_r(result)
}

#' Add Intervals to Forecast
#'
#' @param forecast A Forecast object
#' @param intervals Interval data structure
#' @return Updated Forecast object
#' @export
add_intervals <- function(forecast, intervals) {
  check_setup()
  JuliaCall::julia_assign("fc", forecast)
  JuliaCall::julia_assign("int_vals", intervals)
  result <- JuliaCall::julia_eval("ForecastBaselines.add_intervals(fc, int_vals)")
  convert_forecast_to_r(result)
}

#' Add Trajectories to Forecast
#'
#' @param forecast A Forecast object
#' @param trajectories Matrix of forecast trajectories (rows=samples, cols=horizons)
#' @return Updated Forecast object
#' @export
add_trajectories <- function(forecast, trajectories) {
  check_setup()
  JuliaCall::julia_assign("fc", forecast)
  JuliaCall::julia_assign("traj_vals", as.matrix(trajectories))
  result <- JuliaCall::julia_eval("ForecastBaselines.add_trajectories(fc, traj_vals)")
  convert_forecast_to_r(result)
}

#' Add Temporal Information to Forecast
#'
#' @param forecast A Forecast object
#' @param reference_date Reference date for the forecast
#' @param target_date Target dates for forecast horizons
#' @param resolution Time resolution
#' @return Updated Forecast object
#' @export
add_temporal_info <- function(forecast, reference_date, target_date, resolution) {
  check_setup()
  JuliaCall::julia_assign("fc", forecast)
  JuliaCall::julia_assign("ref_date", reference_date)
  JuliaCall::julia_assign("tgt_date", target_date)
  JuliaCall::julia_assign("res", resolution)
  result <- JuliaCall::julia_eval("ForecastBaselines.add_temporal_info(fc, ref_date, tgt_date, res)")
  convert_forecast_to_r(result)
}

# Filter/Subset Functions

#' Truncate Forecast Horizon
#'
#' Keeps only forecast horizons up to max_h.
#'
#' @param forecast A Forecast object
#' @param max_h Maximum horizon to keep
#' @return Truncated Forecast object
#' @export
truncate_horizon <- function(forecast, max_h) {
  check_setup()
  JuliaCall::julia_assign("fc", forecast)
  JuliaCall::julia_assign("max_horizon", as.integer(max_h))
  result <- JuliaCall::julia_eval("ForecastBaselines.truncate_horizon(fc, max_horizon)")
  convert_forecast_to_r(result)
}

#' Filter Forecast Horizons
#'
#' Keeps only specified forecast horizons.
#'
#' @param forecast A Forecast object
#' @param horizons Vector of horizons to keep
#' @return Filtered Forecast object
#' @export
filter_horizons <- function(forecast, horizons) {
  check_setup()
  JuliaCall::julia_assign("fc", forecast)
  JuliaCall::julia_assign("h_vals", as.integer(horizons))
  result <- JuliaCall::julia_eval("ForecastBaselines.filter_horizons(fc, h_vals)")
  convert_forecast_to_r(result)
}

#' Filter Forecast Levels
#'
#' Keeps only specified confidence levels.
#'
#' @param forecast A Forecast object
#' @param levels Vector of confidence levels to keep
#' @return Filtered Forecast object
#' @export
filter_levels <- function(forecast, levels) {
  check_setup()
  JuliaCall::julia_assign("fc", forecast)
  JuliaCall::julia_assign("lvl_vals", as.numeric(levels))
  result <- JuliaCall::julia_eval("ForecastBaselines.filter_levels(fc, lvl_vals)")
  convert_forecast_to_r(result)
}
