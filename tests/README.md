# ForecastBaselineR Test Suite

This directory contains the test suite for ForecastBaselineR using the `testthat` package.

## Prerequisites

Before running tests, ensure you have:

1. **Julia** (>= 1.9) installed and on your PATH
2. **testthat** R package installed:
   ```r
   install.packages("testthat")
   ```
3. **ForecastBaselineR** package loaded
4. **ForecastBaselines.jl** Julia package installed (happens automatically on first setup)

## Running Tests

### Run all tests

From R console:

```r
library(testthat)
library(ForecastBaselineR)

# Initialize Julia and ForecastBaselines.jl
setup_ForecastBaselines()

# Run all tests
test_check("ForecastBaselineR")
```

Or from command line:

```bash
R CMD check .
```

### Run specific test files

```r
library(testthat)
library(ForecastBaselineR)
setup_ForecastBaselines()

# Run specific test file
test_file("tests/testthat/test-models.R")
test_file("tests/testthat/test-forecast.R")
test_file("tests/testthat/test-scoring.R")
```

### Run tests during package development

If you're developing the package:

```r
devtools::test()
```

## Test Structure

The test suite is organized into the following files:

### `test-setup.R`
- Package loading and initialization
- Function existence checks
- Basic setup validation

### `test-models.R`
- All 10 forecasting models
- Model creation and fitting
- Point forecast generation
- Model-specific behavior

Tests for:
- ConstantModel
- MarginalModel
- KDEModel
- LSDModel
- OLSModel
- IDSModel
- STLModel
- ARMAModel
- INARCHModel
- ETSModel

### `test-forecast.R`
- Complete forecasting workflow
- Interval methods (NoInterval, EmpiricalInterval, ParametricInterval, ModelTrajectoryInterval)
- Multiple horizons
- Multiple confidence levels
- Forecast object structure
- Truth values integration

### `test-scoring.R`
- All scoring rules
- Point forecast scores (MAE, RMSE, MSE, MAPE, etc.)
- Probabilistic scores (CRPS, WIS)
- Calibration diagnostics (PIT, CvM)
- Score calculation with truth values

### `test-utils.R`
- Forecast utility functions
- Query functions (has_mean, has_intervals, etc.)
- Modification functions (add_truth, add_median, etc.)
- Filtering functions (truncate_horizon, filter_horizons, filter_levels)

### `test-transformations.R`
- All transformation types
- Transform/inverse transform operations
- Transformed model workflow
- Box-Cox transformations

## Test Coverage

The test suite covers:

- ✅ All 10 forecasting models
- ✅ All interval methods
- ✅ All scoring rules
- ✅ All transformation types
- ✅ Core forecasting workflow
- ✅ Utility and helper functions
- ✅ Error handling
- ✅ Edge cases

## Writing New Tests

When adding new tests:

1. **Use the skip helper**: All tests that require Julia should use `skip_if_no_julia()`
   ```r
   skip_if_no_julia <- function() {
     if (!is_setup()) {
       skip("Julia is not set up. Run setup_ForecastBaselines() to run these tests.")
     }
   }
   ```

2. **Follow naming conventions**: Test files should be named `test-*.R`

3. **Group related tests**: Use `test_that()` blocks for each specific test

4. **Use descriptive test names**: Test descriptions should clearly state what is being tested

5. **Include expectations**: Each test should have at least one expectation

Example template:

```r
test_that("Feature X works correctly", {
  skip_if_no_julia()

  # Setup
  data <- c(1, 2, 3, 4, 5)
  model <- SomeModel()

  # Execute
  result <- some_function(data, model)

  # Assert
  expect_true(!is.null(result))
  expect_type(result, "double")
  expect_length(result, 5)
})
```

## Continuous Integration

These tests can be integrated into CI/CD pipelines:

```yaml
# Example GitHub Actions workflow
- name: Run tests
  run: |
    Rscript -e 'library(testthat); library(ForecastBaselineR); setup_ForecastBaselines(); test_check("ForecastBaselineR")'
```

## Troubleshooting

### Tests fail with "Julia is not set up"

**Solution**: Run `setup_ForecastBaselines()` before running tests:

```r
library(ForecastBaselineR)
setup_ForecastBaselines()
testthat::test_check("ForecastBaselineR")
```

### Tests are slow on first run

This is normal. Julia uses JIT compilation, so the first test run compiles functions. Subsequent runs will be much faster.

### Tests fail with Julia errors

**Solution**: Check that ForecastBaselines.jl is installed correctly:

```r
setup_ForecastBaselines(verbose = TRUE, install_package = TRUE)
```

### Individual test fails

Run the specific test file with verbose output:

```r
testthat::test_file("tests/testthat/test-models.R", reporter = "progress")
```

## Test Statistics

Expected test counts (approximate):
- **test-setup.R**: 8 tests (basic checks)
- **test-models.R**: 20+ tests (model creation and fitting)
- **test-forecast.R**: 30+ tests (forecasting workflow)
- **test-scoring.R**: 25+ tests (scoring rules)
- **test-utils.R**: 15+ tests (utility functions)
- **test-transformations.R**: 20+ tests (transformations)

**Total**: 100+ comprehensive tests

## Contributing

When contributing new features:

1. Add corresponding tests
2. Ensure all existing tests pass
3. Aim for high test coverage
4. Document any new test patterns

Run tests before submitting pull requests:

```r
devtools::test()
devtools::check()
```
