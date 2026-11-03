# One dataset, one scenario, and the aggregation of a scenario across its
# independent datasets.

run_one_simulation <- function(scenario, sim_id, cfg, prior_cache) {
  generated <- generate_scenario(scenario, cfg)
  dat <- generated$data
  truth <- generated$truth
  rankings <- run_filters(
    dat,
    target = "y",
    cfg = cfg,
    prior_cache = prior_cache
  ) %>%
    mutate(scenario = scenario, sim_id = sim_id)
  # Recovery, stability and selection frequency read the screened ranking; the
  # evidence and Boruta decision counts below read the unscreened one.
  selected <- apply_evidence_screen(rankings)
  recovery <- bind_rows(
    recovery_metrics(
      selected,
      truth,
      top_k = cfg$top_k_core,
      endpoint = "top25_relevant_recovery"
    ),
    recovery_metrics(
      selected,
      truth,
      top_k = cfg$top_k_all,
      endpoint = "top30_all_relevant_recovery"
    )
  ) %>%
    mutate(scenario = scenario, sim_id = sim_id)
  classification_top25 <- classification_metrics_topk(
    selected,
    truth,
    top_k = cfg$top_k_core,
    endpoint = "top25_relevant_definition"
  ) %>%
    mutate(scenario = scenario, sim_id = sim_id)
  rbr_thr <- rbr_threshold_metrics(rankings, truth) %>%
    mutate(scenario = scenario, sim_id = sim_id)
  boruta_decisions <- boruta_decision_metrics(rankings, truth) %>%
    mutate(scenario = scenario, sim_id = sim_id)
  top50_rankings <- selected %>%
    filter(rank <= cfg$save_top_n)
  list(
    truth = truth,
    recovery = recovery,
    classification_top25 = classification_top25,
    rbr_threshold = rbr_thr,
    boruta_decisions = boruta_decisions,
    rankings = selected,
    top50_rankings = top50_rankings
  )
}
summarise_scenario_results <- function(scenario_results, cfg, scenario) {
  truth <- scenario_results[[1]]$truth
  recovery_detail <- bind_rows(map(scenario_results, "recovery"))
  classification_top25_detail <- bind_rows(
    map(scenario_results, "classification_top25")
  )
  rbr_threshold_detail <- bind_rows(map(scenario_results, "rbr_threshold"))
  boruta_decision_detail <- bind_rows(map(scenario_results, "boruta_decisions"))
  # Full rankings are used in memory for stability and selection frequency.
  rankings_detail_full <- bind_rows(map(scenario_results, "rankings"))
  # Only top 50 rankings are saved to Excel.
  top50_rankings_detail <- bind_rows(map(scenario_results, "top50_rankings"))
  recovery_summary <- recovery_detail %>%
    group_by(scenario, endpoint, top_k, method) %>%
    summarise(
      number_independent_datasets = n(),
      total_informative_features = first(total_informative_features),
      total_redundant_features = first(total_redundant_features),
      total_relevant_features = first(total_relevant_features),
      total_noise_features = first(total_noise_features),
      mean_selected_informative = mean(selected_informative),
      sd_selected_informative = sd(selected_informative),
      mean_selected_redundant = mean(selected_redundant),
      sd_selected_redundant = sd(selected_redundant),
      mean_selected_relevant = mean(selected_relevant),
      sd_selected_relevant = sd(selected_relevant),
      mean_selected_noise = mean(selected_noise),
      sd_selected_noise = sd(selected_noise),
      mean_missed_relevant = mean(missed_relevant),
      sd_missed_relevant = sd(missed_relevant),
      mean_informative_TPR = mean(informative_TPR),
      mean_redundant_capture_rate = mean(redundant_capture_rate),
      mean_relevant_TPR = mean(relevant_TPR),
      mean_relevant_precision = mean(relevant_precision),
      mean_noise_FDR = mean(noise_FDR),
      mean_rank_first_noise = mean(rank_first_noise),
      .groups = "drop"
    )
  classification_top25_summary <- classification_top25_detail %>%
    group_by(scenario, endpoint, top_k, method) %>%
    summarise(
      number_independent_datasets = n(),
      total_relevant_features = first(total_relevant_features),
      total_noise_features = first(total_noise_features),
      mean_selected_informative = mean(selected_informative),
      mean_selected_redundant = mean(selected_redundant),
      mean_selected_relevant = mean(selected_relevant),
      mean_selected_noise = mean(selected_noise),
      mean_missed_relevant = mean(missed_relevant),
      mean_TP_relevant = mean(TP_relevant),
      mean_FP_relevant = mean(FP_relevant),
      mean_FN_relevant = mean(FN_relevant),
      mean_TN_relevant = mean(TN_relevant),
      mean_relevant_accuracy = mean(relevant_accuracy),
      mean_relevant_sensitivity = mean(relevant_sensitivity),
      mean_relevant_precision = mean(relevant_precision),
      mean_relevant_specificity = mean(relevant_specificity),
      mean_relevant_F1 = mean(relevant_F1, na.rm = TRUE),
      mean_selected_mismatch_count = mean(selected_mismatch_count),
      sd_selected_mismatch_count = sd(selected_mismatch_count),
      mean_full_confusion_mismatch_count = mean(full_confusion_mismatch_count),
      sd_full_confusion_mismatch_count = sd(full_confusion_mismatch_count),
      number_top25_all_relevant = sum(all_selected_topk_are_relevant),
      percentage_top25_all_relevant =
        100 * mean(all_selected_topk_are_relevant),
      number_top25_contains_noise = sum(selected_topk_contains_noise),
      percentage_top25_contains_noise =
        100 * mean(selected_topk_contains_noise),
      .groups = "drop"
    )
  stability_summary <- stability_metrics(
    rankings_detail_full,
    p = cfg$p_inf + cfg$p_red + cfg$p_noise,
    top_k = cfg$top_k_core,
    endpoint = "top25_relevant_recovery"
  ) %>%
    mutate(scenario = scenario)
  selection_frequency_detail <- bind_rows(
    selection_frequency(
      rankings_detail_full,
      truth,
      cfg$top_k_core,
      endpoint = "top25_relevant_recovery"
    ),
    selection_frequency(
      rankings_detail_full,
      truth,
      cfg$top_k_all,
      endpoint = "top30_all_relevant_recovery"
    )
  ) %>%
    mutate(scenario = scenario)
  selection_frequency_summary <- selection_frequency_detail %>%
    group_by(scenario, endpoint, top_k, method, truth) %>%
    summarise(
      mean_selection_frequency = mean(selection_frequency),
      sd_selection_frequency = sd(selection_frequency),
      .groups = "drop"
    )
  rbr_threshold_summary <- rbr_threshold_detail %>%
    group_by(scenario) %>%
    summarise(
      number_independent_datasets = n(),
      mean_RBR_RBlt1_informative = mean(RBR_RBlt1_informative),
      mean_RBR_RBlt1_redundant = mean(RBR_RBlt1_redundant),
      mean_RBR_RBlt1_noise = mean(RBR_RBlt1_noise),
      mean_RBR_RBlt1_informative_TPR = mean(RBR_RBlt1_informative_TPR),
      mean_RBR_RBlt1_relevant_TPR = mean(RBR_RBlt1_relevant_TPR),
      mean_RBR_RBlt1_noise_rate = mean(RBR_RBlt1_noise_rate),
      mean_RBR_RBgt1_noise = mean(RBR_RBgt1_noise),
      mean_RBR_RBgt1_informative = mean(RBR_RBgt1_informative),
      mean_RBR_RBgt1_noise_correct_rate = mean(RBR_RBgt1_noise_correct_rate),
      .groups = "drop"
    )
  boruta_decision_summary <- boruta_decision_detail %>%
    group_by(scenario, Boruta_decision, truth) %>%
    summarise(
      mean_n_features = mean(n_features),
      sd_n_features = sd(n_features),
      .groups = "drop"
    )
  list(
    truth = truth,
    recovery_detail = recovery_detail,
    recovery_summary = recovery_summary,
    classification_top25_detail = classification_top25_detail,
    classification_top25_summary = classification_top25_summary,
    stability_summary = stability_summary,
    selection_frequency_detail = selection_frequency_detail,
    selection_frequency_summary = selection_frequency_summary,
    rbr_threshold_detail = rbr_threshold_detail,
    rbr_threshold_summary = rbr_threshold_summary,
    boruta_decision_detail = boruta_decision_detail,
    boruta_decision_summary = boruta_decision_summary,
    top50_rankings_detail = top50_rankings_detail
  )
}
#' Run every scenario for one design cell.
#'
#' @param scenarios The scenarios to run, which may be a subset.
#' @param all_scenarios The FULL scenario list from the configuration file. A
#'   dataset's seed is derived from its scenario's position in this list, not in
#'   `scenarios`, so running a subset gives exactly the datasets that running
#'   everything would have given.
run_simulation_study <- function(cfg, scenarios, prior_cache,
                                 all_scenarios = scenarios,
                                 continue_on_error = TRUE) {
  unknown <- setdiff(scenarios, all_scenarios)
  if (length(unknown) > 0) {
    stop(
      "Scenario(s) not in the configured list: ",
      paste(unknown, collapse = ", "),
      call. = FALSE
    )
  }

  dir.create(cfg$output_dir, showWarnings = FALSE, recursive = TRUE)
  all_scenario_outputs <- list()
  for (scenario in scenarios) {
    scenario_index <- match(scenario, all_scenarios)
    scenario_dir <- file.path(cfg$output_dir, scenario)
    # Resume support: skip scenarios whose report already exists on disk.
    # Delete the scenario folder (or just the xlsx) to force a re-run.
    if (file.exists(scenario_report_path(cfg, scenario))) {
      message("Skipping scenario (already completed): ", scenario,
              " [cell ", cfg$cell, "]")
      all_scenario_outputs[scenario] <- list(NULL)
      next
    }
    dir.create(scenario_dir, showWarnings = FALSE, recursive = TRUE)
    message("============================================================")
    message("Cell ", cfg$cell, " (", cfg$cell_role, "): n = ", cfg$n,
            ", p = ", cfg$p, " (", cfg$p_inf, " inf / ", cfg$p_red,
            " red / ", cfg$p_noise, " noise)")
    message("Starting scenario: ", scenario)
    message("Independent datasets for this scenario: ", cfg$n_sims)
    message("No checkpoints will be saved.")
    message("The scenario will be saved when it finishes.")
    message("Output folder: ", scenario_dir)
    message("============================================================")
    scenario_output <- tryCatch({
      scenario_results <- vector("list", cfg$n_sims)
      for (sim_id in seq_len(cfg$n_sims)) {
        message(
          "Cell ", cfg$cell,
          ": scenario = ", scenario,
          ", independent dataset = ", sim_id,
          " of ", cfg$n_sims
        )
        set.seed(dataset_seed(cfg, scenario_index, sim_id))
        scenario_results[[sim_id]] <- run_one_simulation(
          scenario = scenario,
          sim_id = sim_id,
          cfg = cfg,
          prior_cache = prior_cache
        )
      }
      result_list <- summarise_scenario_results(
        scenario_results,
        cfg,
        scenario
      )
      saved_dir <- write_scenario_outputs(
        result_list,
        cfg,
        scenario
      )
      message("Finished and saved scenario: ", scenario)
      message("Files saved to: ", saved_dir)
      rm(scenario_results)
      gc()
      result_list
    }, error = function(e) {
      error_file <- file.path(
        scenario_dir,
        paste0(scenario, "_ERROR.txt")
      )
      writeLines(
        c(
          paste("Cell:", cfg$cell),
          paste("Scenario failed:", scenario),
          paste("Error message:", conditionMessage(e))
        ),
        con = error_file
      )
      message("Scenario failed: ", scenario)
      message("Error saved to: ", error_file)
      if (continue_on_error) {
        return(NULL)
      } else {
        stop(e)
      }
    })
    all_scenario_outputs[[scenario]] <- scenario_output
  }
  invisible(all_scenario_outputs)
}
