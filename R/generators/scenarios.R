# The synthetic data-generating scenarios.
#
# Each returns list(data, truth): a data frame of p standardised features plus
# the factor target `y`, and a table saying which features are informative,
# redundant-but-relevant, or noise. Feature names carry the truth, so nothing
# downstream has to track it positionally.
#
# Citations for the mechanisms, for the paper:
#   mean_shift_independent / *_meanshift : Fan & Fan (2008, Ann. Statist. 36)
#   correlated_gaussian_blocks           : Fan & Lv  (2008, JRSS-B 70)
#   overall synthetic-FS framework       : Bolon-Canedo et al. (2013, KAIS 34)

# ---- Core / existing -------------------------------------------------------
gen_mean_shift_independent <- function(cfg, delta = 1.0, red_rho = 0.90) {
  y <- make_y(cfg$n, cfg$class_prob)
  n <- length(y)
  X_inf <- matrix(NA_real_, nrow = n, ncol = cfg$p_inf)
  for (j in seq_len(cfg$p_inf)) {
    X_inf[, j] <- rnorm(
      n,
      mean = ifelse(y == "2", delta, 0),
      sd = 1
    )
  }
  X_red <- add_redundant_relevant(X_inf, cfg$p_red, red_rho = red_rho)
  X_noise <- matrix(rnorm(n * cfg$p_noise), nrow = n, ncol = cfg$p_noise)
  assemble_dataset(X_inf, X_red, X_noise, y, cfg)
}
gen_correlated_gaussian_blocks <- function(
    cfg, delta = 0.80, rho = 0.65, block_size = 5, red_rho = 0.90) {
  y <- make_y(cfg$n, cfg$class_prob)
  n1 <- sum(y == "1")
  n2 <- sum(y == "2")
  X_inf <- matrix(NA_real_, nrow = cfg$n, ncol = cfg$p_inf)
  X_inf[y == "1", ] <- r_block_normal(
    n1, cfg$p_inf, rho = rho, block_size = block_size,
    mean_vector = rep(0, cfg$p_inf)
  )
  X_inf[y == "2", ] <- r_block_normal(
    n2, cfg$p_inf, rho = rho, block_size = block_size,
    mean_vector = rep(delta, cfg$p_inf)
  )
  X_red <- add_redundant_relevant(X_inf, cfg$p_red, red_rho = red_rho)
  X_noise <- r_block_normal(
    cfg$n, cfg$p_noise, rho = 0.30, block_size = 20
  )
  assemble_dataset(X_inf, X_red, X_noise, y, cfg)
}
# ---- Distributional location scenarios -------------------------------------
# heavy_tailed_meanshift
# Fan & Fan (2008) two-class location design, but with heavy-tailed (Student-t,
# df = 3) within-class marginals instead of Gaussian. Informative features carry
# a class location shift = delta; noise features are i.i.d. t_3. This stresses
# the within-class Gaussian (normal-gamma) assumption of RBR directly: the
# location signal is real, but the tails violate normality. Discretisation-based
# filters (IG/SU) may degrade on heavy tails, giving RBR a chance to lead.
gen_heavy_tailed_meanshift <- function(cfg, delta = 1.0, df = 3, red_rho = 0.90) {
  y <- make_y(cfg$n, cfg$class_prob)
  n <- length(y)
  X_inf <- matrix(NA_real_, nrow = n, ncol = cfg$p_inf)
  for (j in seq_len(cfg$p_inf)) {
    shift <- ifelse(y == "2", delta, 0)
    X_inf[, j] <- shift + rt(n, df = df)
  }
  X_red <- add_redundant_relevant(X_inf, cfg$p_red, red_rho = red_rho)
  X_noise <- matrix(rt(n * cfg$p_noise, df = df), nrow = n, ncol = cfg$p_noise)
  assemble_dataset(X_inf, X_red, X_noise, y, cfg)
}
# skewed_meanshift
# Right-skewed (log-normal) within-class marginals with a class shift in the
# log-mean. On the raw scale this is a genuine marginal location difference
# (E[X | class 2] > E[X | class 1]) but with strong positive skew, mimicking
# gene-expression-style data. Base design: Fan & Fan (2008); skew motivation:
# microarray simulation practice (cf. Tibshirani et al. 2002).
gen_skewed_meanshift <- function(cfg, delta = 0.60, sdlog = 0.50, red_rho = 0.90) {
  y <- make_y(cfg$n, cfg$class_prob)
  n <- length(y)
  X_inf <- matrix(NA_real_, nrow = n, ncol = cfg$p_inf)
  for (j in seq_len(cfg$p_inf)) {
    meanlog <- ifelse(y == "2", delta, 0)
    X_inf[, j] <- rlnorm(n, meanlog = meanlog, sdlog = sdlog)
  }
  X_red <- add_redundant_relevant(X_inf, cfg$p_red, red_rho = red_rho)
  X_noise <- matrix(
    rlnorm(n * cfg$p_noise, meanlog = 0, sdlog = sdlog),
    nrow = n, ncol = cfg$p_noise
  )
  assemble_dataset(X_inf, X_red, X_noise, y, cfg)
}
# mixture_meanshift
# Each informative feature is bimodal WITHIN a class (a two-component Gaussian
# mixture with modes at +/- sep/2), and class 2 is shifted by delta. The
# marginal class means differ (so RBR has location signal), but the bimodality
# inflates within-class variance, making this a deliberately HARDER location
# problem. This is a constructed stress variant (cf. Fan & Fan 2008), not a
# named benchmark -- present it as such in the paper.
gen_mixture_meanshift <- function(
    cfg, delta = 1.0, sep = 2.5, mix_p = 0.50,
    comp_sd = 0.70, red_rho = 0.90) {
  y <- make_y(cfg$n, cfg$class_prob)
  n <- length(y)
  X_inf <- matrix(NA_real_, nrow = n, ncol = cfg$p_inf)
  for (j in seq_len(cfg$p_inf)) {
    comp  <- rbinom(n, size = 1, prob = mix_p)          # mixture component
    base  <- ifelse(comp == 1, sep / 2, -sep / 2)        # bimodal location
    shift <- ifelse(y == "2", delta, 0)                  # class location shift
    X_inf[, j] <- base + shift + rnorm(n, sd = comp_sd)
  }
  X_red <- add_redundant_relevant(X_inf, cfg$p_red, red_rho = red_rho)
  X_noise <- matrix(rnorm(n * cfg$p_noise), nrow = n, ncol = cfg$p_noise)
  assemble_dataset(X_inf, X_red, X_noise, y, cfg)
}
generate_scenario <- function(scenario, cfg) {
  switch(
    scenario,
    mean_shift_independent       = gen_mean_shift_independent(cfg),
    correlated_gaussian_blocks   = gen_correlated_gaussian_blocks(cfg),
    heavy_tailed_meanshift       = gen_heavy_tailed_meanshift(cfg),
    skewed_meanshift             = gen_skewed_meanshift(cfg),
    mixture_meanshift            = gen_mixture_meanshift(cfg),
    stop("Unknown scenario: ", scenario)
  )
}
