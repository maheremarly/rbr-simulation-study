# Per-dataset recovery and mismatch metrics.
#
# "Relevant" always means informative plus redundant-relevant. Two endpoints are
# reported: the top 25, against the 25 informative features, and the top 30,
# against all 30 relevant ones.
#
# Recovery and stability are measured on the evidence-screened RBR ranking:
# features carrying evidence against equality of the class means, RB < 1, are
# kept and ordered by strength before the top-k is taken. The comparison methods
# have no such rule and are ranked as they stand.


#' Keep only the RBR features whose relative belief ratio is below one.
#'
#' The surviving features keep their order and are renumbered from 1, so that a
#' top-k cut means the same thing for every method. Rows from the other methods
#' pass through untouched.
#'
#' @param rankings Ranking rows from run_filters().
#' @param screened_method The method the evidence rule applies to.
apply_evidence_screen <- function(rankings, screened_method = "RBR") {
  rankings %>%
    filter(method != screened_method | (!is.na(RB_lt_1) & RB_lt_1)) %>%
    group_by(method) %>%
    arrange(rank, .by_group = TRUE) %>%
    mutate(rank = row_number()) %>%
    ungroup()
}

