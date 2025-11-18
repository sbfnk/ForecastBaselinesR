#' Setup Julia and ForecastBaselines
#'
#' Configures Julia and installs required Julia packages for ForecastBaselineR.
#' This function should be run on first use if automatic setup fails.
#'
#' @param JULIA_HOME Path to Julia installation (optional, will auto-detect if not provided)
#' @param install_package Whether to install ForecastBaselines.jl if not already installed (default: TRUE)
#' @param rebuild Whether to rebuild the Julia system image (default: FALSE)
#' @param verbose Logical. If TRUE, prints progress messages. Default is TRUE.
#'
#' @return Invisible TRUE if setup succeeds, otherwise throws an error.
#'
#' @examples
#' \dontrun{
#' # Basic setup (auto-detect Julia)
#' setup_ForecastBaselines()
#'
#' # Specify Julia location
#' setup_ForecastBaselines(JULIA_HOME = "/usr/local/julia/bin")
#'
#' # Silent setup
#' setup_ForecastBaselines(verbose = FALSE)
#' }
#'
#' @export
setup_ForecastBaselines <- function(JULIA_HOME = NULL,
                                    install_package = TRUE,
                                    rebuild = FALSE,
                                    verbose = TRUE) {

  if (verbose) message("[1/5] Setting up Julia...")

  # Setup Julia
  if (verbose) message("[2/5] Calling JuliaCall::julia_setup()...")
  if (is.null(JULIA_HOME)) {
    JuliaCall::julia_setup(rebuild = rebuild, verbose = verbose)
  } else {
    JuliaCall::julia_setup(JULIA_HOME = JULIA_HOME, rebuild = rebuild, verbose = verbose)
  }

  if (verbose) message("[3/5] Julia initialized successfully")

  # Install ForecastBaselines.jl if requested
  if (install_package) {
    if (verbose) message("[4/5] Checking ForecastBaselines.jl installation...")

    # Try to load the package, install if it fails
    tryCatch({
      JuliaCall::julia_eval("using ForecastBaselines")
      if (verbose) message("[4/5] ForecastBaselines.jl is already installed")
    }, error = function(e) {
      if (verbose) {
        message("[4/5] Installing ForecastBaselines.jl from GitHub...")
        message("       This may take a few minutes on first install...")
      }
      JuliaCall::julia_eval("using Pkg")
      JuliaCall::julia_eval('Pkg.add(url="https://github.com/ManuelStapper/ForecastBaselines.jl")')

      # Precompile to avoid runtime errors
      if (verbose) message("       Precompiling packages (this may take a while)...")
      tryCatch({
        JuliaCall::julia_eval("Pkg.precompile()")
        if (verbose) message("       Precompilation complete")
      }, error = function(e) {
        if (verbose) {
          message("Warning: Precompilation failed: ", conditionMessage(e))
          message("Continuing anyway - packages will compile on first use")
        }
      })

      JuliaCall::julia_eval("using ForecastBaselines")
      if (verbose) message("[4/5] ForecastBaselines.jl installed successfully")
    })
  } else {
    # Just try to load
    if (verbose) message("[4/5] Loading ForecastBaselines.jl...")
    JuliaCall::julia_eval("using ForecastBaselines")
  }

  # Test connection
  if (verbose) message("[5/5] Testing Julia connection...")
  test_result <- JuliaCall::julia_eval("1 + 1")

  if (test_result == 2) {
    if (verbose) message("[5/5] ForecastBaselineR setup complete!")
    invisible(TRUE)
  } else {
    stop("Julia setup failed validation test")
  }
}

#' Check if Julia and ForecastBaselines are available
#'
#' Tests whether Julia is configured and ForecastBaselines.jl is accessible.
#'
#' @return Logical. TRUE if Julia and ForecastBaselines are available, FALSE otherwise.
#'
#' @examples
#' \dontrun{
#' if (forecastbaselines_available()) {
#'   # Run forecasting analysis
#' } else {
#'   setup_ForecastBaselines()
#' }
#' }
#'
#' @export
forecastbaselines_available <- function() {
  tryCatch(
    {
      # Check if Julia is available and ForecastBaselines is loaded
      JuliaCall::julia_eval("isdefined(Main, :ForecastBaselines)")
    },
    error = function(e) {
      # Julia not available at all
      FALSE
    }
  )
}

#' @keywords internal
#' @export
is_setup <- function() {
  forecastbaselines_available()
}

# Internal function to check setup and give helpful error message
check_setup <- function() {
  if (!is_setup()) {
    stop("Julia and ForecastBaselines.jl are not set up. Please run setup_ForecastBaselines() first.",
         call. = FALSE)
  }
}

#' @keywords internal
.onLoad <- function(libname, pkgname) {
  # Try automatic setup
  tryCatch(
    {
      # Setup Julia connection (don't install Julia automatically in .onLoad)
      JuliaCall::julia_setup()

      # Try to load ForecastBaselines.jl if it exists
      JuliaCall::julia_eval("using ForecastBaselines")

      packageStartupMessage("ForecastBaselines Julia backend loaded successfully")
    },
    error = function(e) {
      packageStartupMessage("ForecastBaselineR: R interface to ForecastBaselines.jl")
      packageStartupMessage("Julia setup incomplete. Run setup_ForecastBaselines() to configure.")
    }
  )
}
