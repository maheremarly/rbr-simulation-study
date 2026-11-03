# Packages, module loading and seeding.
#
# Scripts are run from the repository root (see docs/reproducing.md), so modules
# are sourced by their path relative to it.


REQUIRED_PACKAGES <- c(
  "yaml", "dplyr", "tidyr", "purrr", "ggplot2", "FSelector",
  "praznik", "CORElearn", "Boruta", "openxlsx"
)


#' Stop with one actionable message if any required package is missing.
check_packages <- function(packages = REQUIRED_PACKAGES) {
  missing <- packages[
    !vapply(packages, requireNamespace, logical(1), quietly = TRUE)
  ]
  if (length(missing) > 0) {
    stop(
      "Install the following package(s) before running: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  invisible(TRUE)
}


#' Attach the packages whose functions are called unqualified.
load_packages <- function() {
  check_packages()
  suppressPackageStartupMessages({
    library(dplyr)
    library(tidyr)
    library(purrr)
    library(ggplot2)
    library(FSelector)
    library(praznik)
    library(CORElearn)
    library(Boruta)
    library(openxlsx)
  })
  invisible(TRUE)
}


#' Source the project's modules, checking first that the cwd is the repo root.
source_modules <- function(root = ".") {
  if (!dir.exists(file.path(root, "R"))) {
    stop(
      "R/ not found. Run this script from the repository root, ",
      "for example: Rscript scripts/run_simulation.R --cells A",
      call. = FALSE
    )
  }
  modules <- c(
    "R/utils/args.R",
    "R/utils/config.R",
    "R/generators/helpers.R",
    "R/generators/scenarios.R",
    "R/filters/prior.R",
    "R/filters/rbr.R",
    "R/filters/baselines.R",
    "R/utils/metrics.R",
    "R/utils/stability.R",
    "R/utils/outputs.R",
    "R/utils/simulation.R"
  )
  for (module in modules) source(file.path(root, module))
  invisible(TRUE)
}


#' Set the random seed.
set_seed <- function(seed) {
  set.seed(seed)
  invisible(seed)
}
