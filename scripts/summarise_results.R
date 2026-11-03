#!/usr/bin/env Rscript
# Collect the per-scenario reports into one cross-cell workbook.
#
# Reads the saved reports rather than in-memory results, so a resumed run is
# summarised in full: scenarios completed in an earlier run are included
# alongside the ones just finished.

source("R/utils/setup.R")
source_modules()


# The report sheets worth carrying across cells, and the name each gets in the
# combined workbook.
SUMMARY_SHEETS <- c(
  recovery_summary        = "recovery_summary",
  class_top25_summary     = "classification_summary",
  stability_top25         = "stability_summary",
  rbr_threshold_summary   = "rbr_threshold_summary"
)


#' Read one sheet from every scenario report of every cell.
#'
#' Missing reports are reported and skipped, so a partial study still
#' summarises.
collect_sheet <- function(config, cells, scenarios, sheet, output_root) {
  rows <- list()

  for (cell_name in cells) {
    cfg <- make_cell_config(cell_name, config, output_root)

    for (scenario in scenarios) {
      path <- scenario_report_path(cfg, scenario)
      if (!file.exists(path)) next

      table <- tryCatch(
        openxlsx::read.xlsx(path, sheet = sheet),
        error = function(e) NULL
      )
      if (is.null(table) || nrow(table) == 0) next

      table$cell <- cell_name
      table$cell_role <- cfg$cell_role
      table$n <- cfg$n
      table$p <- cfg$p
      rows[[paste(cell_name, scenario, sep = "/")]] <- table
    }
  }

  if (length(rows) == 0) return(NULL)
  dplyr::bind_rows(rows)
}


#' Build and write the combined workbook.
summarise_results <- function(config, cells, scenarios, output_root) {
  missing <- character(0)
  for (cell_name in cells) {
    cfg <- make_cell_config(cell_name, config, output_root)
    for (scenario in scenarios) {
      if (!file.exists(scenario_report_path(cfg, scenario))) {
        missing <- c(missing, paste0(cell_name, "/", scenario))
      }
    }
  }
  if (length(missing) > 0) {
    message(
      "No report yet for ", length(missing), " cell/scenario combination(s): ",
      paste(utils::head(missing, 6), collapse = ", "),
      if (length(missing) > 6) ", ..." else ""
    )
  }

  combined <- list()
  for (i in seq_along(SUMMARY_SHEETS)) {
    sheet <- names(SUMMARY_SHEETS)[i]
    table <- collect_sheet(config, cells, scenarios, sheet, output_root)
    if (is.null(table)) {
      message("Nothing found for sheet '", sheet, "'")
      next
    }
    combined[[SUMMARY_SHEETS[[i]]]] <- table
    message("  ", SUMMARY_SHEETS[[i]], ": ", nrow(table), " rows")
  }

  if (length(combined) == 0) {
    stop(
      "No reports found under ", output_root,
      ". Run scripts/run_simulation.R first.",
      call. = FALSE
    )
  }

  path <- file.path(output_root, "cross_cell_summary.xlsx")
  openxlsx::write.xlsx(combined, file = path, overwrite = TRUE)
  message("Cross-cell summary written to ", path)

  invisible(combined)
}


main <- function() {
  args <- parse_args(list(
    config     = "configs/simulation.yml",
    cells      = NULL,
    scenarios  = NULL,
    output_dir = "output"
  ))

  check_packages()
  config <- load_config(args$config)

  cells <- if (is.null(args$cells)) {
    names(config$cells)
  } else {
    trimws(strsplit(args$cells, ",")[[1]])
  }
  scenarios <- if (is.null(args$scenarios)) {
    unlist(config$scenarios)
  } else {
    trimws(strsplit(args$scenarios, ",")[[1]])
  }

  summarise_results(config, cells, scenarios, args$output_dir)
}


if (sys.nframe() == 0) main()
