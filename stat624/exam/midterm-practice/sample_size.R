sample_size <- function(err, sigma_0, alpha = 0.05) {
  n <- 1
  conditions_met <- FALSE
  error <- err + 1

  while (error > err) {
    n <- n + 1
    error <- qt(1-alpha/2, n-1) * sigma_0 / sqrt(n)

  }

  n
}

sample_size(5, 6.24)
