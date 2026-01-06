set.seed(1337)
library(parallel)
cores <- 8

# Generate N permutation tests of 1 sample
paired_permutation_test <- function(x, y, n_permutations) {
  d <- cbind(x, y)
  test_statistic <- t.test(d[, 1], d[, 2], paired = TRUE)$statistic
  n <- nrow(d)
  # Remember, a paired t-test is a just a t-test on the differences.
  diff <- d[, 1] - d[, 2] # Precompute the differences for speed
  dist_of_test_statistic <- sapply(seq_len(n_permutations), \(i) {
    d <- diff * sample(c(-1, 1), n, replace = TRUE) # Multiply by zero to "flip"
    mean(d) / sqrt(var(d) / n) # Compute the test statistic as fast as possible
  })
  mean(abs(dist_of_test_statistic) >= abs(test_statistic))
}

# Generate 1 paired t test p value
paired_t_test <- function(x, y) t.test(x, y, paired = TRUE)$p.value

# Calculate 1 Power sampling of one sample (including paired test and n permutations)
power <- function(x1, x2, n_replications, n_permutations, alpha = 0.05) {
  y <- lapply(seq_len(n_replications), \(i) {
    c(paired_permutation_test(x1, x2, n_permutations), paired_t_test(x1, x2))
  })
  perm_mean <- colMeans(Reduce(rbind, y) <= alpha)[1]
  paired_mean <- colMeans(Reduce(rbind, y) <= alpha)[2]
  perm_sd <- sd(Reduce(rbind, y)[, 1])
  #colMeans(Reduce(rbind, y) <= alpha)
  c(paired_mean, perm_mean, perm_sd)
}

# Function to run through tests for all parameter configurations
gen_test <- function(test_params, n_replications, n_permutations, alpha) {
  n <- test_params[1, 1]
  diff <- test_params[1, 2]
  type <- test_params[1, 3]

  if (type == 0) {
    x <- rnorm(n, 0, 1)
    y <- rnorm(n, diff, 1)
  } else {
    a1 <- .1
    a2 <- 1
    mean <- a1 / (a1 + a2)
    sd <- sqrt(a1 * a2) / ((a1 + a2)^2 * (a1 + a2 + 1))
    x <- ((rbeta(n, a1, a2) - mean) / sd) * 1
    y <- ((rbeta(n, a1, a2) - mean) / sd) * 1 + diff
  }

  power(x, y, n_replications, n_permutations)
}


# Overall test parameters
N <- c(10, 20, 35)
mean_diff = c(0, .5, 1)
test_type <- c(0, 1) # 0 is Normal, 1 is Skewed
test_params <- expand.grid(N, mean_diff, test_type)

# Define replication parameters
alpha <- .05
n_rep <- 1000
n_perm <- 1000

# Execute over all possible combos of parameters
powers <- mclapply(seq_len(nrow(test_params)), function(i) {
  row <- test_params[i,]
  gen_test(row, n_replications = n_rep, n_permutations = n_perm, alpha = alpha)
}, mc.cores = cores)


# Output the results
for (i in 1:nrow(test_params)) {
  n <- test_params[i, 1]
  diff <- test_params[i, 2]
  type <- test_params[i, 3]
  paired_power <- powers[[i]][1]
  perm_power <- powers[[i]][2]
  perm_sd <- powers[[i]][3]
  perm_lower <- perm_power - qnorm(.975) * perm_sd / sqrt(n_rep)
  perm_upper <- perm_power + qnorm(.975) * perm_sd / sqrt(n_rep)
  cat("Type:", type, "| Diff:", diff, "| n:", n, "| Paired:", round(paired_power, 4), "| Perm:", round(perm_power, 4), "| Lower:", round(perm_lower, 4), "| Upper:", round(perm_upper, 4), "\n")
}


