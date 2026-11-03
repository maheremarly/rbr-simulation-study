# The five methods the RBR filter is compared against, and the single call
# that runs all six on one dataset.
#
# Every ranker returns the same long format -- method, feature, rank, score --
# so the metrics in R/utils/ never need to know which method produced a row.
# Ties are broken by feature name, so a ranking is a deterministic function of
# the scores and not of the column order.
#
# ReliefF and Boruta run at their package settings. Where they score poorly it
# is a high-dimensional failure mode rather than a fault: ReliefF's neighbour
# distances are swamped by the noise dimensions, and Boruta is conservative.

score_to_rank <- function(scores, method, decreasing = TRUE) {
  scores <- as.data.frame(scores)
  out <- tibble(
    method = method,
    feature = rownames(scores),
    score = as.numeric(scores[[1]])
  )
  if (decreasing) {
    out <- out %>% arrange(desc(score), feature)
  } else {
    out <- out %>% arrange(score, feature)
  }
  out %>%
    mutate(rank = row_number()) %>%
    select(method, feature, rank, score)
}
rank_mrmr <- function(dat, target = "y") {
  xnames <- setdiff(names(dat), target)
  mrmr_out <- praznik::MRMR(
    dat[, xnames, drop = FALSE],
    dat[[target]],
    length(xnames),
    threads = 0
  )
  d_mrmr <- mrmr_out$score
  mrmr_features <- names(d_mrmr)
  if (is.null(mrmr_features) || any(mrmr_features == "") ||
      !all(mrmr_features %in% xnames)) {
    if (!is.null(mrmr_out$selection)) {
      mrmr_features <- xnames[as.integer(mrmr_out$selection)]
    } else {
      stop("mRMR output did not contain usable names or selection indices.")
    }
  }
  missing_features <- setdiff(xnames, mrmr_features)
  if (length(missing_features) > 0) {
    mrmr_features <- c(mrmr_features, missing_features)
    d_mrmr <- c(as.numeric(d_mrmr), rep(NA_real_, length(missing_features)))
  }
  tibble(
    method = "mRMR",
    feature = mrmr_features,
    rank = seq_along(mrmr_features),
    score = as.numeric(d_mrmr)
  )
}
# ReliefF runs at CORElearn's settings. Its modest showing at large p is a
# curse-of-dimensionality effect: the neighbour distances it depends on are
# dominated by the noise features. kNearestExpRank and ReliefIterations are the
# knobs that change its behaviour, neither of which recovers signal that is
# invisible marginally.
rank_relieff <- function(dat, target = "y", relief_iterations = 100) {
  relief_scores <- CORElearn::attrEval(
    as.formula(paste(target, "~ .")),
    dat,
    estimator = "ReliefFexpRank",
    ReliefIterations = relief_iterations
  )
  tibble(
    method = "ReliefF",
    feature = names(relief_scores),
    score = as.numeric(relief_scores)
  ) %>%
    arrange(desc(score), feature) %>%
    mutate(rank = row_number()) %>%
    select(method, feature, rank, score)
}
# Boruta runs at its package settings. It confirms few features at small n,
# where no single feature's random forest importance clears the shadow-max
# threshold, which is the conservatism the method is designed around.
rank_boruta <- function(dat, target = "y", maxRuns = 100) {
  fit <- Boruta::Boruta(
    as.formula(paste(target, "~ .")),
    data = dat,
    maxRuns = maxRuns,
    doTrace = 0,
    holdHistory = TRUE
  )
  fit_fixed <- tryCatch(
    Boruta::TentativeRoughFix(fit),
    error = function(e) fit
  )
  att <- Boruta::attStats(fit_fixed)
  tibble(
    method = "Boruta",
    feature = rownames(att),
    score = as.numeric(att$meanImp),
    Boruta_decision = as.character(att$decision)
  ) %>%
    mutate(
      decision_order = match(Boruta_decision, c("Confirmed", "Tentative", "Rejected")),
      decision_order = ifelse(is.na(decision_order), 4, decision_order)
    ) %>%
    arrange(decision_order, desc(score), feature) %>%
    mutate(rank = row_number()) %>%
    select(method, feature, rank, score, Boruta_decision)
}
run_filters <- function(dat, target = "y", cfg, prior_cache) {
  rbr_raw <- RBR_filter(
    dat,
    target_variable = target,
    B_rbr = cfg$B_rbr,
    prior_cache = prior_cache
  )
  rbr_rank <- rbr_raw %>%
    transmute(
      method = "RBR",
      feature = Feature,
      score = -strength,
      strength = strength,
      RB_dist = RB_dist,
      RB_lt_1 = RB_dist < 1
    ) %>%
    arrange(strength, RB_dist, feature) %>%
    mutate(rank = row_number()) %>%
    select(method, feature, rank, score, strength, RB_dist, RB_lt_1)
  ig <- FSelector::information.gain(as.formula(paste(target, "~ .")), dat)
  ig_rank <- score_to_rank(
    ig,
    method = "Information_Gain",
    decreasing = TRUE
  )
  su <- FSelector::symmetrical.uncertainty(as.formula(paste(target, "~ .")), dat)
  su_rank <- score_to_rank(
    su,
    method = "Symmetric_Uncertainty",
    decreasing = TRUE
  )
  mrmr_rank <- rank_mrmr(dat, target = target)
  relief_rank <- rank_relieff(
    dat,
    target = target,
    relief_iterations = cfg$relief_iterations
  )
  boruta_rank <- rank_boruta(
    dat,
    target = target,
    maxRuns = cfg$boruta_maxRuns
  )
  bind_rows(
    rbr_rank,
    ig_rank,
    su_rank,
    mrmr_rank,
    relief_rank,
    boruta_rank
  )
}
