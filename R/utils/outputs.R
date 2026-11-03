# Writing one scenario report: the multi-sheet workbook and the two plots.

scenario_report_path <- function(cfg, scenario) {
  file.path(
    cfg$output_dir,
    scenario,
    paste0(scenario, "_simulation_report.xlsx")
  )
}
write_scenario_outputs <- function(result_list, cfg, scenario) {
  scenario_dir <- file.path(cfg$output_dir, scenario)
  dir.create(scenario_dir, showWarnings = FALSE, recursive = TRUE)
  prefix <- scenario
  excel_file <- scenario_report_path(cfg, scenario)
  sheets <- list(
    truth = result_list$truth,
    recovery_detail = result_list$recovery_detail,
    recovery_summary = result_list$recovery_summary,
    class_top25_detail = result_list$classification_top25_detail,
    class_top25_summary = result_list$classification_top25_summary,
    stability_top25 = result_list$stability_summary,
    selection_frequency = result_list$selection_frequency_detail,
    selection_freq_summary = result_list$selection_frequency_summary,
    rbr_threshold_detail = result_list$rbr_threshold_detail,
    rbr_threshold_summary = result_list$rbr_threshold_summary,
    boruta_decision_detail = result_list$boruta_decision_detail,
    boruta_decision_summary = result_list$boruta_decision_summary,
    top50_rankings = result_list$top50_rankings_detail
  )
  openxlsx::write.xlsx(
    sheets,
    file = excel_file,
    overwrite = TRUE
  )
  plot_recovery_top25 <- result_list$recovery_summary %>%
    filter(endpoint == "top25_relevant_recovery") %>%
    select(
      method,
      mean_selected_informative,
      mean_selected_redundant,
      mean_selected_noise
    ) %>%
    pivot_longer(
      cols = starts_with("mean_selected"),
      names_to = "feature_group",
      values_to = "mean_count"
    ) %>%
    mutate(
      feature_group = recode(
        feature_group,
        mean_selected_informative = "Informative",
        mean_selected_redundant = "Redundant relevant",
        mean_selected_noise = "Noise"
      )
    ) %>%
    ggplot(aes(x = method, y = mean_count, fill = feature_group)) +
    geom_col(position = "dodge") +
    labs(
      title = paste0("Top-25 recovery: ", scenario,
                     " (cell ", cfg$cell, ": n=", cfg$n, ", p=", cfg$p, ")"),
      subtitle = "Relevant features are informative + redundant relevant features",
      x = "Feature selection method",
      y = "Mean number of selected features",
      fill = "Feature group"
    ) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 35, hjust = 1))
  ggsave(
    file.path(scenario_dir, paste0(prefix, "_plot_top25_recovery.png")),
    plot_recovery_top25,
    width = 12,
    height = 6
  )
  plot_stability_top25 <- result_list$stability_summary %>%
    select(method, Jaccard, Kuncheva, Spearman) %>%
    pivot_longer(
      cols = c(Jaccard, Kuncheva, Spearman),
      names_to = "metric",
      values_to = "value"
    ) %>%
    ggplot(aes(x = method, y = value)) +
    geom_col() +
    facet_wrap(~ metric, scales = "free_y") +
    labs(
      title = paste0("Top-25 stability across independent datasets: ", scenario,
                     " (cell ", cfg$cell, ": n=", cfg$n, ", p=", cfg$p, ")"),
      x = "Feature selection method",
      y = "Mean stability value"
    ) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 35, hjust = 1))
  ggsave(
    file.path(scenario_dir, paste0(prefix, "_plot_top25_stability.png")),
    plot_stability_top25,
    width = 12,
    height = 6
  )
  invisible(scenario_dir)
}
