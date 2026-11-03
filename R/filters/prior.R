# Elicitation of the conjugate prior, and the prior Monte Carlo cache.
#
# The prior is identical for every feature, every dataset and every cell, so its
# Monte Carlo sample is drawn ONCE per run and reused. That removes prior-side
# Monte Carlo noise from every comparison between cells, and removes most of the
# cost of scoring a large feature set.
#
# The elicitation inputs are those the paper states for STANDARDISED data.

# The four elicitation inputs are stated for standardised data: every feature
# is scaled to pooled mean 0 and unit variance before it is scored. The mean
# range (-1, 1) and the variance bounds 1.900 and 5.699 give
#
#   mu0 = 0, lambda0 ~ 0.419, alpha0 ~ 9.82, beta0 ~ 7.83
#
# with mu0 and lambda0 in closed form and the shape and rate from the Newton
# solve, whose tolerance leaves them free in the third decimal.
rbr_prior_hyperparameters <- function() {
  elicit <- function(a, b, alpha, s1_sqr, s2_sqr) {
    c(
      qgamma((1 + alpha) / 2, a, b) -
        ((1 / s1_sqr) * qnorm((1 + alpha) / 2))^2,
      qgamma((1 - alpha) / 2, a, b) -
        ((1 / s2_sqr) * qnorm((1 + alpha) / 2))^2
    )
  }
  m1 <- -1
  m2 <- 1
  s1_sqr <- 1.900
  s2_sqr <- 5.699
  alpha <- 0.999
  x <- c(2, 2)
  delta <- x
  maxiter <- 1000
  iter <- 0
  tol <- 0.5
  while (sqrt(sum(delta^2)) > tol && iter < maxiter) {
    h <- 0.000000001
    fx <- elicit(x[1], x[2], alpha, s1_sqr, s2_sqr)
    Jx <- matrix(0, 2, 2)
    for (i in 1:2) {
      xh <- x
      xh[i] <- x[i] + h
      Jx[, i] <- (elicit(xh[1], xh[2], alpha, s1_sqr, s2_sqr) - fx) / h
    }
    delta <- -solve(Jx) %*% elicit(x[1], x[2], alpha, s1_sqr, s2_sqr)
    x <- as.numeric(x + delta)
    iter <- iter + 1
  }
  if (iter >= maxiter) stop("RBR prior elicitation did not converge.")
  list(
    alpha0 = x[1],
    beta0 = x[2],
    lambda0 = sqrt((((m2 - m1) / 2)^2) / s2_sqr),
    mu0 = (m1 + m2) / 2
  )
}
build_rbr_prior_cache <- function(B_rbr = 5000, M_rbr = 100) {
  hp <- rbr_prior_hyperparameters()
  var_prior1 <- 1 / rgamma(B_rbr, hp$alpha0, rate = hp$beta0)
  var_prior2 <- 1 / rgamma(B_rbr, hp$alpha0, rate = hp$beta0)
  mu_prior1 <- rnorm(B_rbr, hp$mu0, hp$lambda0 * sqrt(var_prior1))
  mu_prior2 <- rnorm(B_rbr, hp$mu0, hp$lambda0 * sqrt(var_prior2))
  sum_var <- (1 / var_prior1) + (1 / var_prior2)
  mu_prior_T <- ((mu_prior1 / var_prior1) + (mu_prior2 / var_prior2)) / sum_var
  d_prior <- (
    ((mu_prior_T - mu_prior1)^2 / var_prior1) +
      ((mu_prior_T - mu_prior2)^2 / var_prior2)
  ) / 2
  psi_prior <- c(
    0,
    as.numeric(quantile(d_prior, seq_len(M_rbr) / M_rbr, names = FALSE))
  )
  list(
    hp = hp,
    B_rbr = B_rbr,
    M_rbr = M_rbr,
    psi_prior = psi_prior,
    psi_star = psi_prior[2],
    prior_mass_star = mean(d_prior <= psi_prior[2])
  )
}
# Guard on the elicited hyperparameters. mu0 and lambda0 are closed form and
# must hold exactly; alpha0 and beta0 come from the solver and are reported
# rather than asserted, since its tolerance leaves the third decimal free.
check_rbr_prior <- function(hp) {
  message(sprintf(
    "RBR prior: mu0 = %.6f (expect ~0.000), lambda0 = %.6f (expect ~0.419)",
    hp$mu0, hp$lambda0
  ))
  message(sprintf(
    "RBR prior solver output: alpha0 = %.4f (expect ~9.82), beta0 = %.4f (expect ~7.83)",
    hp$alpha0, hp$beta0
  ))
  if (abs(hp$mu0) > 1e-8 || abs(hp$lambda0 - 0.419) > 0.005) {
    stop("RBR prior check failed: mu0 and lambda0 are not the elicited values.")
  }
  invisible(TRUE)
}
