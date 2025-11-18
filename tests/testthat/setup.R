# Test setup and helpers
# This file is automatically loaded before all tests

# Skip helper for Julia/ForecastBaselines availability
skip_if_no_julia <- function() {
  testthat::skip_if_not(is_setup(), "Julia/ForecastBaselines not available. Run setup_ForecastBaselines() first.")
}

# Helper to check if ForecastBaselines is available
forecastbaselines_available <- function() {
  is_setup()
}

# Sample data for tests
sample_data <- c(10, 12, 13, 14, 15, 16, 18, 19, 20, 22, 21, 23)
truth_values <- c(24, 25, 26)
