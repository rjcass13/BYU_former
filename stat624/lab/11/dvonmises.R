dvonmises <- function(x, mu, nu, kappa1, kappa2, lambda, log = FALSE, use_normalizing_constant = TRUE) {
  # If we don't want to return with the normalizing constant, just set it to 1
  if (use_normalizing_constant) {
    # The ratio used in the sum. Calculate outside
    inner_ratio <- (lambda^2 / (4 * kappa1 * kappa2))
    c_sum <- 0
    sum_curr <- 0
    sum_next <- 1
    m = 0
    iter <- 1
    # Stop the loop if we've done 100 (just a value I picked) 
    # or the change is smaller than some small number (another I chose)
    while ((abs(sum_next - sum_curr) >= 1e-6) & (iter < 100)) {
      sum_curr <- sum_next
      sum_next <- choose(2 * m, m) * inner_ratio^m * besselI(kappa1, m) * besselI(kappa2, m)
      c_sum <- c_sum + sum_next
      iter <- iter + 1
      m <- m + 1
    }
    c_inv <- 4 * pi^2 * c_sum
  } else {
    c_inv = 1
  }

  # Calculate the density
  if (log) {
    log(1/c_inv) + kappa1 * cos(x[1] - mu) + kappa2 * cos(x[2] - nu) + lambda * sin(x[1] - mu) * sin(x[2] - nu)
  } else {
    1/c_inv * exp(kappa1 * cos(x[1] - mu) + kappa2 * cos(x[2] - nu) + lambda * sin(x[1] - mu) * sin(x[2] - nu))
  }
  # If log is requested, return the log. Else the regular
}
