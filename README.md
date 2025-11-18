# ForecastBaselinesR

An R package providing comprehensive baseline forecasting models with uncertainty quantification and evaluation tools.

## Overview

ForecastBaselinesR provides access to 10 baseline forecasting models through a clean, intuitive R interface. The package supports:

- **10 Forecasting Models**: From simple baselines to advanced time series models
- **Probabilistic Forecasting**: Multiple methods for prediction intervals
- **Comprehensive Scoring**: Point forecast metrics and probabilistic scores
- **Data Transformations**: Log, power, and Box-Cox transformations
- **Calibration Diagnostics**: PIT functions and Cramér-von Mises divergence

## Quick Start

```r
# Install
devtools::install_local("ForecastBaselinesR")

# Setup (first time per R session)
library(ForecastBaselinesR)
setup_ForecastBaselines()

# Create and fit model
data <- c(10, 12, 13, 14, 15, 16, 18, 19, 20, 22)
model <- ARMAModel(p = 1, q = 1)
fitted <- fit_baseline(data, model)

# Generate probabilistic forecast
fc <- forecast(
  fitted,
  interval_method = EmpiricalInterval(n_trajectories = 1000),
  horizon = 1:5,
  levels = c(0.80, 0.95)
)

# Evaluate
truth <- c(23, 24, 25, 26, 27)
fc_eval <- add_truth(fc, truth)
mae <- score(fc_eval, MAE())
crps <- score(fc_eval, CRPS())
```

## Available Models

### Simple Baseline Models
- **ConstantModel**: Naive forecast using last observed value
- **MarginalModel**: Forecasts based on empirical marginal distribution
- **KDEModel**: Kernel density estimation

### Seasonal/Trend Models
- **LSDModel**: Last Similar Dates method (seasonal patterns)
- **OLSModel**: Ordinary least squares with polynomial trends
- **IDSModel**: Increase-Decrease-Stable trend detection
- **STLModel**: Seasonal-Trend decomposition using Loess

### Advanced Time Series Models
- **ARMAModel**: Autoregressive Moving Average
- **INARCHModel**: Integer-valued ARCH for count data
- **ETSModel**: Error-Trend-Season exponential smoothing (all 30 variants)

## Installation

### Prerequisites

1. **Julia** (>= 1.9): Download from [julialang.org](https://julialang.org/downloads/)
2. **R** (>= 3.5.0)
3. **JuliaCall R package**: `install.packages("JuliaCall")`

### Install ForecastBaselinesR

```r
# Install devtools if needed
install.packages("devtools")

# Install ForecastBaselinesR
devtools::install_local("ForecastBaselinesR")
```

## Documentation

- **[ForecastBaselinesR/README.md](ForecastBaselinesR/README.md)**: Comprehensive package documentation
- **[ForecastBaselinesR/INSTALL.md](ForecastBaselinesR/INSTALL.md)**: Detailed installation instructions
- **[ForecastBaselinesR/QUICKSTART.md](ForecastBaselinesR/QUICKSTART.md)**: Quick start guide
- **[examples/](ForecastBaselinesR/examples/)**: Working example scripts

## Examples

### Seasonal Forecasting

```r
# Monthly data with yearly seasonality
model <- STLModel(s = 12, robust = TRUE)
fitted <- fit_baseline(monthly_data, model)
fc <- forecast(
  fitted,
  interval_method = EmpiricalInterval(n_trajectories = 2000),
  horizon = 1:12,
  levels = c(0.80, 0.95)
)
```

### Model Comparison

```r
models <- list(
  Naive = ConstantModel(),
  ARMA = ARMAModel(p = 2, q = 1),
  STL = STLModel(s = 12)
)

results <- lapply(names(models), function(name) {
  fitted <- fit_baseline(data, models[[name]])
  fc <- forecast(fitted, horizon = 1:12, truth = truth)
  data.frame(
    Model = name,
    MAE = score(fc, MAE()),
    CRPS = score(fc, CRPS())
  )
})

comparison <- do.call(rbind, results)
print(comparison)
```

### Data Transformation

```r
# Fit to log-transformed data
model <- ARMAModel(p = 1)
trans <- LogTransform()
model_trans <- transform_model(model, trans)

fitted <- fit_baseline(data, model_trans)
# Forecasts automatically back-transformed
fc <- forecast(fitted, horizon = 1:10)
```

## Features

### Uncertainty Quantification

Multiple interval methods:
- **NoInterval**: Point forecasts only
- **EmpiricalInterval**: Bootstrap from historical errors
- **ParametricInterval**: Model-based parametric intervals
- **ModelTrajectoryInterval**: Simulate from fitted model

### Scoring Rules

**Point Forecast Scores**:
- MAE, MdAE, MAPE
- MSE, MSPE, RMSE
- Bias, RelativeBias

**Probabilistic Scores**:
- CRPS (Continuous Ranked Probability Score)
- WIS (Weighted Interval Score)
- CRPS from trajectories

### Calibration Assessment

- PIT (Probability Integral Transform) functions
- Cramér-von Mises divergence

## Repository Structure

```
.
├── ForecastBaselinesR/     # R package
│   ├── R/                  # R functions
│   ├── examples/           # Example scripts
│   ├── README.md           # Package documentation
│   ├── INSTALL.md          # Installation guide
│   ├── QUICKSTART.md       # Quick start guide
│   └── DESCRIPTION         # Package metadata
├── LICENSE.txt             # MIT License
└── README.md               # This file
```

## License

MIT License - see [LICENSE.txt](LICENSE.txt) for details

## Citation

If you use this package in your research, please cite:

```bibtex
@software{forecastbaselines,
  title = {ForecastBaselinesR: Baseline Forecasting Models for R},
  year = {2024},
  url = {https://github.com/sbfnk/ForecastBaselines.jl}
}
```

## Contributing

Contributions are welcome! Please open an issue or pull request on GitHub.

## Support

For bugs and feature requests, please open an issue on [GitHub](https://github.com/sbfnk/ForecastBaselines.jl/issues).

## Getting Started

1. **Install**: Follow the [installation guide](ForecastBaselinesR/INSTALL.md)
2. **Quick Start**: Try the [quick start guide](ForecastBaselinesR/QUICKSTART.md)
3. **Examples**: Run scripts in [examples/](ForecastBaselinesR/examples/)
4. **Documentation**: Read the [package README](ForecastBaselinesR/README.md)
