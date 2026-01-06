set.seed(1337)

# Calculate the proportion of sames
sames <- function(d) {
  n <- length(d)
  same <- 0

  for (i in 1:(n-1)) {
    if (d[i] == d[i+1]) {
      same <- same + 1
    }
  }

  same/(n-1)
}

# Simulation Study to determine distirbution of sames proportion
B <- 10000
n <- 30
p1 <- .5
p2 <- .5
props <- numeric(B)
for (i in 1:B) {
  samp <- sample(c(0,1), n, replace = TRUE, c(p1, p2))
  props[i] <- sames(samp)
}

# Calculate sample population mean/sd
mu_hat <- mean(props)
sigma_hat <- sd(props)

# Initial observed data
d_obs <- c(0, 1, 0, 1, 1, 0, 1, 0, 0, 1, 1, 1, 1, 1, 0, 1, 0, 1, 1, 1, 0, 1, 0, 1, 1, 0, 1, 1, 1, 1)
T_obs  <- sames(d_obs)

# Calculate p-value of our observed dsitribution
(mu_hat - T_obs) - qt(.95, n-1, lower.tail = TRUE) * sigma_hat / sqrt(n)
