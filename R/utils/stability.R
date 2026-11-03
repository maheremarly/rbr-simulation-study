# Selection stability across independently generated datasets.
#
# Stability is measured over the full set of dataset pairs, not a sample of
# them: with n_sims = 300 that is 44,850 pairs per method per scenario.
#
# Jaccard is the raw overlap of two selected sets. Kuncheva corrects it for the
# overlap expected by chance given p and k, so it is comparable across the cells
# of the design, which have different p.

jaccard_index <- function(A, B) {
  length(intersect(A, B)) / length(union(A, B))
}
kuncheva_index <- function(A, B, p, k) {
  r <- length(intersect(A, B))
  if (k == p) return(NA_real_)
  (r - k^2 / p) / (k - k^2 / p)
}
pairwise_mean <- function(x) {
  if (length(x) == 0) return(NA_real_)
  mean(x, na.rm = TRUE)
}
# Full pairwise stability across independently generated datasets.
# This is intentionally NOT sampled.
stability_metrics <- function(simulation_rankings, p, top_k, endpoint) {
  methods <- unique(simulation_rankings$method)
  map_dfr(methods, function(m) {
    rr <- simulation_rankings %>% filter(method == m)
    ids <- sort(unique(rr$sim_id))
    if (length(ids) < 2) {
      return(tibble(
        method = m,
        endpoint = endpoint,
        top_k = top_k,
        number_dataset_pairs = 0,
        Jaccard = NA_real_,
        Kuncheva = NA_real_,
        Spearman = NA_real_
      ))
    }
    pairs <- combn(ids, 2, simplify = FALSE)
    vals <- map_dfr(pairs, function(pr) {
      r1 <- rr %>% filter(sim_id == pr[1]) %>% arrange(rank)
      r2 <- rr %>% filter(sim_id == pr[2]) %>% arrange(rank)
      s1 <- r1$feature[r1$rank <= top_k]
      s2 <- r2$feature[r2$rank <= top_k]
      common_features <- intersect(r1$feature, r2$feature)
      tibble(
        Jaccard = jaccard_index(s1, s2),
        Kuncheva = kuncheva_index(s1, s2, p = p, k = top_k),
        Spearman = suppressWarnings(cor(
          r1$rank[match(common_features, r1$feature)],
          r2$rank[match(common_features, r2$feature)],
          method = "spearman"
        ))
      )
    })
    vals %>%
      summarise(
        method = m,
        endpoint = endpoint,
        top_k = top_k,
        number_dataset_pairs = nrow(vals),
        Jaccard = pairwise_mean(Jaccard),
        Kuncheva = pairwise_mean(Kuncheva),
        Spearman = pairwise_mean(Spearman)
      )
  })
}
selection_frequency <- function(simulation_rankings, truth, top_k, endpoint) {
  simulation_rankings %>%
    mutate(selected = rank <= top_k) %>%
    group_by(method, feature) %>%
    summarise(selection_frequency = mean(selected), .groups = "drop") %>%
    left_join(truth, by = "feature") %>%
    mutate(endpoint = endpoint, top_k = top_k)
}
