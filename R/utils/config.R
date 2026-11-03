# Reads configs/simulation.yml and derives one design cell's configuration.
#
# n, p, p_noise, the seed and the output directory are never written down: each
# is derived from the cell, so they cannot go stale when a cell is edited.


#' Load the configuration file.
load_config <- function(path = "configs/simulation.yml") {
  if (!file.exists(path)) {
    stop("Configuration file not found: ", path, call. = FALSE)
  }
  config <- yaml::yaml.load_file(path)

  stopifnot(length(config$cells) > 0, length(config$scenarios) > 0)
  config$base$class_prob <- unlist(config$base$class_prob)
  config
}


#' Assertions that must hold for every cell.
#'
#' Stated generally, with no literal n, p or p_noise, so one set of checks
#' covers the whole design.
validate_config <- function(cfg) {
  # Checked before anything is derived from them, so a configuration mistake is
  # reported here rather than surfacing as a strange recovery number later.
  stopifnot(is.numeric(cfg[["n"]]), length(cfg[["n"]]) == 1, cfg[["n"]] > 0)
  stopifnot(is.numeric(cfg[["p"]]), length(cfg[["p"]]) == 1, cfg[["p"]] > 0)

  stopifnot(cfg$p_inf + cfg$p_red + cfg$p_noise == cfg$p)
  stopifnot(cfg$p_noise > 0)
  stopifnot(abs(sum(cfg$class_prob) - 1) < 1e-12)

  n1 <- round(cfg$n * cfg$class_prob["1"])
  n2 <- round(cfg$n * cfg$class_prob["2"])
  stopifnot(n1 + n2 == cfg$n)
  stopifnot(n1 == n2)  # balanced classes throughout the cross design

  stopifnot(cfg$top_k_core == cfg$p_inf)
  stopifnot(cfg$top_k_all == cfg$p_inf + cfg$p_red)
  stopifnot(cfg$save_top_n <= cfg$p)

  invisible(TRUE)
}


#' Derive one cell's complete configuration from the shared base settings.
#'
#' @param cell_name A key of `config$cells`.
#' @param config Output of load_config().
#' @param output_root Directory the cell's own output directory is created in.
#' @return A validated configuration list.
make_cell_config <- function(cell_name, config, output_root = "output") {
  cell <- config$cells[[cell_name]]
  if (is.null(cell)) {
    stop(
      "Unknown cell '", cell_name, "'. Available: ",
      paste(names(config$cells), collapse = ", "),
      call. = FALSE
    )
  }

  # Indexed with [[ ]] rather than $ throughout. `$` partial-matches on lists,
  # so a missing `n` would quietly read `n_sims` instead, and assigning NULL to
  # it would quietly do nothing -- a combination that hides the error entirely.
  for (key in c("n_obs", "p", "role")) {
    if (is.null(cell[[key]])) {
      stop(
        "Cell '", cell_name, "' has no `", key, "`. Cell keys found: ",
        paste(names(cell), collapse = ", "),
        if ("FALSE" %in% names(cell)) {
          "  (a key read as FALSE means a bare `n:` -- write `n_obs:`)"
        } else {
          ""
        },
        call. = FALSE
      )
    }
  }

  cfg <- config$base
  cfg[["cell"]] <- cell_name
  cfg[["cell_role"]] <- cell[["role"]]
  cfg[["n"]] <- cell[["n_obs"]]
  cfg[["p"]] <- cell[["p"]]
  cfg[["p_noise"]] <- cell[["p"]] - cfg[["p_inf"]] - cfg[["p_red"]]

  # Cells are separated by 10^7 so that the per-dataset seeds
  # (seed + scenario_index * 100000 + sim_id) can never collide across cells.
  # The separation depends on the cell's POSITION in configs/simulation.yml.
  cfg[["seed"]] <- cfg[["seed_base"]] +
    match(cell_name, names(config$cells)) * 10000000

  cfg[["output_dir"]] <- file.path(
    output_root,
    sprintf(
      "rbr_binary_cell%s_n%d_p%d_sims%d",
      cell_name, cfg$n, cfg$p, cfg$n_sims
    )
  )

  validate_config(cfg)
  cfg
}


#' Seed for the shared prior cache, which is built once per run.
prior_cache_seed <- function(config) config$base$seed_base + 999


#' Seed for one dataset.
#'
#' `scenario_index` is the scenario's position in the FULL list from the
#' configuration file, not its position in the subset being run, so running a
#' subset of scenarios gives the same datasets as running all of them.
dataset_seed <- function(cfg, scenario_index, sim_id) {
  cfg$seed + scenario_index * 100000 + sim_id
}
