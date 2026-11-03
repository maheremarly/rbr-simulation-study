# The relative belief ratio (RBR) filter for two-class problems.
#
# Each feature is scored by the evidence against H0: mu1 = mu2. The discrepancy
# psi is the symmetrised Kullback-Leibler distance between the two
# class-conditional normals and their precision-weighted pool. psi is drawn from
# the posterior B times and compared against the cached prior sample; the
# relative belief ratio at psi -> 0 is the ratio of the two distribution
# functions at the 1/M prior quantile, and the strength is the posterior mass on
# discrepancies no more surprising than that ratio.
#
# A SMALL strength means strong evidence against equality of the means. A
# feature with zero or undefined within-class variance gets strength Inf, so it
# sorts last rather than erroring.

RBR_filter <- function(d, target_variable, B_rbr = 5000, prior_cache = NULL) {
  d[[target_variable]] <- factor(d[[target_variable]], levels = c("1", "2"))
  if (nlevels(d[[target_variable]]) != 2) {
    stop("RBR_filter requires a binary target variable.")
  }
  if (is.null(prior_cache)) {
    prior_cache <- build_rbr_prior_cache(B_rbr = B_rbr, M_rbr = 100)
  }
  Cal_RB <- function(xbar_sample1, xbar_sample2, s_sqr_sample1, s_sqr_sample2, n1, n2) {
    hp <- prior_cache$hp
    B <- prior_cache$B_rbr
    alpha_post1 <- hp$alpha0 + n1 / 2
    alpha_post2 <- hp$alpha0 + n2 / 2
    beta_post1 <- hp$beta0 + (n1 - 1) * s_sqr_sample1 / 2 +
      n1 * (xbar_sample1 - hp$mu0)^2 / (2 * (n1 * hp$lambda0^2 + 1))
    beta_post2 <- hp$beta0 + (n2 - 1) * s_sqr_sample2 / 2 +
      n2 * (xbar_sample2 - hp$mu0)^2 / (2 * (n2 * hp$lambda0^2 + 1))
    var_post1 <- 1 / rgamma(B, alpha_post1, rate = beta_post1)
    var_post2 <- 1 / rgamma(B, alpha_post2, rate = beta_post2)
    mu_x_post1 <- ((hp$mu0 / hp$lambda0^2) + n1 * xbar_sample1) /
      (n1 + 1 / hp$lambda0^2)
    mu_x_post2 <- ((hp$mu0 / hp$lambda0^2) + n2 * xbar_sample2) /
      (n2 + 1 / hp$lambda0^2)
    mu_post1 <- rnorm(B, mu_x_post1, sqrt(var_post1 / (n1 + 1 / hp$lambda0^2)))
    mu_post2 <- rnorm(B, mu_x_post2, sqrt(var_post2 / (n2 + 1 / hp$lambda0^2)))
    sum_var <- (1 / var_post1) + (1 / var_post2)
    mu_post_T <- ((mu_post1 / var_post1) + (mu_post2 / var_post2)) / sum_var
    d_post <- (
      ((mu_post_T - mu_post1)^2 / var_post1) +
        ((mu_post_T - mu_post2)^2 / var_post2)
    ) / 2
    RB_dist <- mean(d_post <= prior_cache$psi_star) / prior_cache$prior_mass_star
    posterior_cdf <- vapply(
      prior_cache$psi_prior,
      function(boundary) mean(d_post <= boundary),
      numeric(1)
    )
    posterior_bin_mass <- diff(posterior_cdf)
    RBR_hat <- prior_cache$M_rbr * posterior_bin_mass
    strength <- sum(posterior_bin_mass[RBR_hat <= RB_dist])
    c(RB_dist = RB_dist, strength = strength)
  }
  feature_names <- setdiff(names(d), target_variable)
  map_dfr(feature_names, function(col_name) {
    x <- d[[col_name]]
    y <- d[[target_variable]]
    A <- tapply(x, y, mean)
    v <- tapply(x, y, var)
    n <- tapply(x, y, length)
    if (any(is.na(v)) || any(v <= 0)) {
      result <- c(RB_dist = NA_real_, strength = Inf)
    } else {
      result <- Cal_RB(A[1], A[2], v[1], v[2], n[1], n[2])
    }
    tibble(
      Feature = col_name,
      RB_dist = unname(result["RB_dist"]),
      strength = unname(result["strength"])
    )
  })
}
