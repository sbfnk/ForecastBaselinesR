# Quick Start Guide

Get up and running with ForecastBaselinesR in 5 minutes!

## Installation (First Time Only)

```r
# 1. Install JuliaCall
install.packages("JuliaCall")

# 2. Install ForecastBaselinesR
devtools::install_local("/path/to/ForecastBaselinesR")

# 3. Load and setup (this may take a few minutes the first time)
library(ForecastBaselinesR)
setup_ForecastBaselines()
```

**Note**: Make sure you have Julia installed first! See [INSTALL.md](INSTALL.md) for details.

## Basic Usage

### 1. Simple Forecast

```r
library(ForecastBaselinesR)
setup_ForecastBaselines()

# Your data
data <- c(10, 12, 13, 14, 15, 16, 18, 19, 20, 22)

# Create model → Fit → Forecast
model <- ARMAModel(p = 1)
fitted <- fit_baseline(data, model)
forecasts <- point_forecast(fitted, horizon = 1:5)

print(forecasts)
```

### 2. Forecast with Uncertainty

```r
# Generate probabilistic forecast with prediction intervals
fc <- forecast(
  fitted,
  interval_method = EmpiricalInterval(n_trajectories = 1000),
  horizon = 1:5,
  levels = c(0.80, 0.95)
)

print(fc)
```

### 3. Evaluate Forecast Accuracy

```r
# Add true observed values
truth <- c(23, 24, 25, 26, 27)
fc_eval <- add_truth(fc, truth)

# Compute error metrics
mae <- MAE(fc_eval)
rmse <- RMSE(fc_eval)

cat("MAE:", mae, "\n")
cat("RMSE:", rmse, "\n")
```

## Available Models

Choose the model that fits your data:

```r
# Simple baselines
ConstantModel()                    # Last value
MarginalModel(p = 10)             # Mean of last 10 obs

# Trend/Seasonal
LSDModel(s = 12)                  # Seasonal pattern
STLModel(s = 12)                  # STL decomposition
OLSModel(degree = 2)              # Polynomial trend

# Time series models
ARMAModel(p = 1, q = 1)           # ARMA
ETSModel("A", "A", "N")           # Exponential smoothing
INARCHModel(p = 1)                # For count data
```

## Common Tasks

### Seasonal Data (e.g., Monthly)

```r
# Monthly data with yearly seasonality
model <- STLModel(s = 12)
fitted <- fit_baseline(monthly_data, model)
fc <- forecast(fitted, horizon = 1:12,
              interval_method = EmpiricalInterval())
```

### Weekly Data

```r
# Weekly data with 52-week seasonality
model <- LSDModel(s = 52)
fitted <- fit_baseline(weekly_data, model)
fc <- forecast(fitted, horizon = 1:4)
```

### Count Data

```r
# For integer counts (e.g., number of events)
model <- INARCHModel(p = 1)
fitted <- fit_baseline(count_data, model)
fc <- forecast(fitted,
              interval_method = EmpiricalInterval(
                positivity_correction = "post_clip"
              ))
```

### Data Transformation

```r
# Apply log transformation (useful for multiplicative trends)
model <- ARMAModel(p = 1)
trans <- LogTransform()
model_trans <- transform_model(model, trans)

fitted <- fit_baseline(data, model_trans)
# Forecasts are automatically back-transformed!
fc <- forecast(fitted, horizon = 1:10)
```

## Model Comparison

```r
# Compare multiple models
models <- list(
  Naive = ConstantModel(),
  ARMA = ARMAModel(p = 1, q = 1),
  STL = STLModel(s = 12)
)

results <- lapply(names(models), function(name) {
  fitted <- fit_baseline(data, models[[name]])
  fc <- forecast(fitted, horizon = 1:12, truth = truth)
  data.frame(
    Model = name,
    MAE = MAE(fc),
    RMSE = RMSE(fc)
  )
})

comparison <- do.call(rbind, results)
print(comparison)
```

## Interval Methods

Choose how to quantify uncertainty:

```r
# No intervals (point forecast only)
NoInterval()

# Bootstrap from historical errors (non-parametric)
EmpiricalInterval(n_trajectories = 1000, seed = 42)

# Model-based parametric intervals
ParametricInterval()

# Simulate from the model
ModelTrajectoryInterval(n_trajectories = 1000)
```

## Scoring Rules

Evaluate forecast quality:

```r
# Point forecast accuracy
MAE(forecast)               # Mean Absolute Error
RMSE(forecast)              # Root Mean Squared Error
MAPE(forecast)              # Mean Absolute Percentage Error

# Probabilistic accuracy (requires quantile/interval forecasts)
WIS(forecast)               # Weighted Interval Score
CRPS(forecast)              # Continuous Ranked Probability Score

# Get all metrics at once
all_scores <- score(forecast)
```

## Common Patterns

### Pattern 1: Fit-Forecast-Evaluate

```r
# Standard workflow
fitted <- fit_baseline(data, model)
fc <- forecast(fitted, interval_method = EmpiricalInterval(),
              horizon = 1:h, truth = truth)
mae <- MAE(fc)
```

### Pattern 2: Cross-Validation

```r
# Rolling window validation
n <- length(data)
window <- 50
errors <- numeric(n - window - 12)

for (i in 1:(n - window - 12)) {
  train <- data[i:(i + window - 1)]
  test <- data[(i + window):(i + window + 11)]

  fitted <- fit_baseline(train, model)
  fc <- forecast(fitted, horizon = 1:12, truth = test)
  errors[i] <- MAE(fc)
}

cat("Average MAE:", mean(errors), "\n")
```

### Pattern 3: Ensemble Forecasts

```r
# Combine multiple models
models <- list(ARMAModel(p=1), STLModel(s=12), ETSModel("A","A","N"))
forecasts <- lapply(models, function(m) {
  fitted <- fit_baseline(data, m)
  point_forecast(fitted, 1:12)
})

# Simple average ensemble
ensemble <- rowMeans(do.call(cbind, forecasts))
```

## Tips

1. **First Time Setup**: The first run of `setup_ForecastBaselines()` may take a few minutes to install Julia packages.

2. **Speed**: Julia uses JIT compilation, so the first forecast will be slower. Subsequent forecasts are fast!

3. **Model Selection**: Start simple (ConstantModel, MarginalModel) then try more complex models (ARMA, ETS, STL).

4. **Seasonality**: If your data has clear seasonal patterns, use models with `s` parameter (STL, LSD, ARMA with s).

5. **Intervals**: EmpiricalInterval is robust and works well in most cases. Use more trajectories (2000+) for smoother intervals.

6. **Transformations**: Use LogTransform or Box-Cox for data with multiplicative trends or heteroscedasticity.

## Next Steps

- **Full Examples**: Check `examples/basic_usage.R` and `examples/seasonal_forecasting.R`
- **Documentation**: See `README.md` for complete API documentation
- **Installation Help**: See `INSTALL.md` if you encounter setup issues

## Help

```r
# Get help on any function
?ARMAModel
?forecast
?score

# Check if setup is complete
is_setup()
```

## Minimal Working Example

Copy-paste this to test your installation:

```r
library(ForecastBaselinesR)
setup_ForecastBaselines()

set.seed(123)
data <- cumsum(rnorm(50)) + 10
model <- ARMAModel(p = 1)
fitted <- fit_baseline(data, model)
fc <- forecast(fitted, interval_method = EmpiricalInterval(),
              horizon = 1:5, levels = 0.95)
print(fc)
```

If this runs without errors, you're all set! 🎉
