# Building blocks shared by every data-generating scenario: the class labels,
# the truth table, the correlated-normal draw, the redundant-relevant features,
# and the standardise-and-shuffle step every scenario ends with.
#
# Features are standardised to mean 0 and unit variance before scoring, which
# is what the prior in R/filters/prior.R was elicited for.

make_y <- function(n, class_prob) {
  n1 <- round(n * class_prob["1"])
  n2 <- n - n1
  factor(c(rep("1", n1), rep("2", n2)), levels = c("1", "2"))
}
shuffle_data <- function(dat) {
  dat[sample(seq_len(nrow(dat))), , drop = FALSE]
}
make_truth <- function(p_inf, p_red, p_noise) {
  tibble(
    feature = c(
      paste0("inf_", seq_len(p_inf)),
      paste0("red_", seq_len(p_red)),
      paste0("noise_", seq_len(p_noise))
    ),
    truth = c(
      rep("informative", p_inf),
      rep("redundant_relevant", p_red),
      rep("noise", p_noise)
    ),
    all_relevant = c(
      rep(TRUE, p_inf + p_red),
      rep(FALSE, p_noise)
    )
  )
}
standardize_numeric <- function(df, target = "y") {
  xnames <- setdiff(names(df), target)
  df[xnames] <- lapply(df[xnames], function(z) {
    z_sd <- stats::sd(z)
    if (is.na(z_sd) || z_sd <= 0) return(rep(0, length(z)))
    as.numeric(scale(z))
  })
  df
}
r_block_normal <- function(
    n, p, rho = 0.50, block_size = 10,
    mean_vector = rep(0, p), sd_vector = rep(1, p)) {
  stopifnot(length(mean_vector) == p)
  stopifnot(length(sd_vector) == p)
  if (rho < 0 || rho >= 1) stop("rho must be in [0, 1).")
  Z <- matrix(rnorm(n * p), nrow = n, ncol = p)
  blocks <- split(seq_len(p), ceiling(seq_len(p) / block_size))
  for (b in blocks) {
    latent <- rnorm(n)
    Z[, b] <- sqrt(rho) * latent +
      sqrt(1 - rho) * Z[, b, drop = FALSE]
  }
  Z <- sweep(Z, 2, sd_vector, "*")
  sweep(Z, 2, mean_vector, "+")
}
add_redundant_relevant <- function(X_inf, p_red, red_rho = 0.90) {
  X_red <- matrix(NA_real_, nrow = nrow(X_inf), ncol = p_red)
  for (j in seq_len(p_red)) {
    base_j <- ((j - 1) %% ncol(X_inf)) + 1
    X_red[, j] <- red_rho * X_inf[, base_j] +
      sqrt(1 - red_rho^2) * rnorm(nrow(X_inf))
  }
  X_red
}
assemble_dataset <- function(X_inf, X_red, X_noise, y, cfg) {
  colnames(X_inf) <- paste0("inf_", seq_len(cfg$p_inf))
  colnames(X_red) <- paste0("red_", seq_len(cfg$p_red))
  colnames(X_noise) <- paste0("noise_", seq_len(cfg$p_noise))
  dat <- data.frame(X_inf, X_red, X_noise, y = y, check.names = FALSE)
  dat <- standardize_numeric(dat, target = "y")
  list(
    data = shuffle_data(dat),
    truth = make_truth(cfg$p_inf, cfg$p_red, cfg$p_noise)
  )
}
