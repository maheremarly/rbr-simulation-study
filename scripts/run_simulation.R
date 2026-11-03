#!/usr/bin/env Rscript
# Run the simulation study: every scenario, for every design cell.
#
# Scenarios that already have a report on disk are skipped, so an interrupted
# run resumes without redoing finished work. Delete a scenario's folder to
# force it to run again.

source("R/utils/setup.R")
source_modules()


#' Build the prior cache that every cell and scenario shares.
#'
#' Drawn once, from its own fixed seed, so that psi_star and prior_mass_star are
#' identical in every cell. That removes prior-side Monte Carlo noise from the
#' comparisons between cells.
build_shared_prior <- function(config) {
  set_seed(prior_cache_seed(config))
  prior_cache <- build_rbr_prior_cache(
    B_rbr = config$base$B_rbr,
    M_rbr = config$base$M_rbr
  )
  check_rbr_prior(prior_cache$hp)
  prior_cache
}


#' Run the requested scenarios for each requested cell.
run_cells <- function(config, cells, scenarios, output_root, continue_on_error) {
  prior_cache <- build_shared_prior(config)
  all_scenarios <- unlist(config$scenarios)

  results <- list()
  for (cell_name in cells) {
    cfg <- make_cell_config(cell_name, config, output_root)

    message("############################################################")
    message("### CELL ", cell_name, " -- ", cfg$cell_role)
    message("### n = ", cfg$n, ", p = ", cfg$p, ", n_sims = ", cfg$n_sims)
    message("### Output directory: ", cfg$output_dir)
    message("############################################################")

    results[[cell_name]] <- run_simulation_study(
      cfg               = cfg,
      scenarios         = scenarios,
      prior_cache       = prior_cache,
      all_scenarios     = all_scenarios,
      continue_on_error = continue_on_error
    )
  }
  results
}


#' Split a comma-separated argument, or fall back to every available value.
selected <- function(value, available, label) {
  if (is.null(value)) return(available)
  chosen <- trimws(strsplit(value, ",")[[1]])
  unknown <- setdiff(chosen, available)
  if (length(unknown) > 0) {
    stop(
      "Unknown ", label, ": ", paste(unknown, collapse = ", "),
      ". Available: ", paste(available, collapse = ", "),
      call. = FALSE
    )
  }
  chosen
}


main <- function() {
  args <- parse_args(list(
    config        = "configs/simulation.yml",
    cells         = NULL,
    scenarios     = NULL,
    output_dir    = "output",
    stop_on_error = FALSE
  ))

  load_packages()
  config <- load_config(args$config)

  cells <- selected(args$cells, names(config$cells), "cell(s)")
  scenarios <- selected(args$scenarios, unlist(config$scenarios), "scenario(s)")

  message(
    "Cells: ", paste(cells, collapse = " "),
    " | scenarios: ", paste(scenarios, collapse = " ")
  )

  run_cells(
    config            = config,
    cells             = cells,
    scenarios         = scenarios,
    output_root       = args$output_dir,
    continue_on_error = !args$stop_on_error
  )

  message("Done. Reports are under ", args$output_dir, "/")
}


if (sys.nframe() == 0) main()
