plot_mixture_fit <- function(z, theta_hat) {
  # z is the data
  # theta_hat = c(pi_hat, mu1_hat, mu2_hat, sigma2_hat)
  # 1. Create histogram with freq=FALSE
  # 2. Create sequence of x values for plotting
  # 3. Compute mixture density at each x
  # 4. Overlay with lines() or curve()

  p <- theta_hat[1, 1]
  mu1 <- theta_hat[2, 1]
  mu2 <- theta_hat[3, 1]
  sigma2 <- theta_hat[4, 1]

  # Generate x values over full range of z values, plus a little extra in the wings
  x <- seq(min(z) - sd(z), max(z) + sd(z), by = 0.001)
  d <- p * dnorm(x, mu1, sqrt(sigma2)) + (1 - p) * dnorm(x, mu2, sqrt(sigma2))

  hist(z, freq = FALSE, main = 'Shared Var. Dist. w/ Density from Est. Params.', xlim = c(-6, 6), ylim = c(0, 0.3))
  lines(x, d, col = 'red')
}