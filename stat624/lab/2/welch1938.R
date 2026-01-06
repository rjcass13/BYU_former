# Pooled variance t-test (equal variance assumed)
# Returns true is the null hypothesis is rejected
t_test_pool <- function(y1, y2, alpha = 0.05) {
  n1 <- length(y1)
  n2 <- length(y2)
  s12 <- var(y1)
  s22 <- var(y2)
  df <- n1 + n2 - 2
  s.pool <- sqrt(((n1 - 1) * s12 + (n2 - 1) * s22) / (n1 + n2 - 2))
  test.statistic <- (mean(y1) - mean(y2)) / (s.pool * sqrt(1.0 / n1 + 1.0 / n2))
  abs(test.statistic) >= qt(1 - alpha / 2, df)
}

# Separate variance test (Law of Large Numbers applied)
# Returns true is the null hypothesis is rejected
t_test_asymptotic <- function(y1, y2, alpha = 0.05) {
  n1 <- length(y1)
  n2 <- length(y2)
  s12 <- var(y1)
  s22 <- var(y2)
  test.statistic <- (mean(y1) - mean(y2)) / sqrt(s12 / n1 + s22 / n2)
  abs(test.statistic) >= qnorm(1 - alpha / 2)
}

# Welch approximation t-test (degrees of freedom modified)
# Returns true is the null hypothesis is rejected
t_test_welch <- function(y1, y2, alpha = 0.05) {
  n1 <- length(y1)
  n2 <- length(y2)
  s12 <- var(y1)
  s22 <- var(y2)
  df <- (s12 / n1 + s22 / n2)^2 /
    ((s12 / n1)^2 / (n1 - 1) + (s22 / n2)^2 / (n2 - 1))
  test.statistic <- (mean(y1) - mean(y2)) / sqrt(s12 / n1 + s22 / n2)
  abs(test.statistic) >= qt(1 - alpha / 2, df)
}

contestants <- c(
  pooled = t_test_pool,
  asymptotic = t_test_asymptotic,
  welch = t_test_welch
)

# Simulation parameters
set.seed(234)
n1 <- 6
n2 <- 10
mu1 <- 0.0
sigma1 <- 1.0
mu2 <- c(0.0, 0.5, 1.0, 2.0)
sigma2 <- c(0.25, 1.0, 2.0, 5.0)
nreps <- 1000
alpha <- 0.10

# For each test, do the following nreps times: 
# Generate a sample for y1 and y2 (repeat for each distribution for y2)
# run the test and see if the means overlap. Record result
for (dist in 1:4) {
  rejections_pool = 0 
  rejections_asymptotic = 0 
  rejections_welch = 0 
  for (rep in 1:nreps) {
    # Get the Y1 and Y2 distributions
    y1 <- rnorm(n1, mu1, sqrt(sigma1))
    y2 <- rnorm(n2, mu2[dist], sqrt(sigma2[dist]))

    # Run the 3 tests for this set of Y1 and Y2
    if (t_test_pool(y1, y2, alpha) == TRUE) {
      rejections_pool = rejections_pool + 1
    }
    if (t_test_asymptotic(y1, y2, alpha) == TRUE) {
      rejections_asymptotic = rejections_asymptotic + 1
    }
    if (t_test_welch(y1, y2, alpha) == TRUE) {
      rejections_welch = rejections_welch + 1
    }
  }

  # Calculate rejection ratios
  rejection_ratio_pool = rejections_pool/nreps
  rejection_ratio_asymptotic = rejections_asymptotic/nreps
  rejection_ratio_welch = rejections_welch/nreps

  # Calculate Monte Carle Errors
  me_pool <- round(qnorm(0.975) * sqrt(rejection_ratio_pool * (1 - rejection_ratio_pool) / nreps), 3)
  me_asymptotic <- round(qnorm(0.975) * sqrt(rejection_ratio_asymptotic * (1 - rejection_ratio_asymptotic) / nreps), 3)
  me_welch <- round(qnorm(0.975) * sqrt(rejection_ratio_welch * (1 - rejection_ratio_welch) / nreps), 3)
  
  cat("Distribution - mu:", mu2[dist], ", var:", sigma2[dist], "\n")
  cat("Pooled Rejection Ratio:", rejection_ratio_pool, "- MCE:", me_pool, "\n")
  cat("Asymptotic Rejection Ratio:", rejection_ratio_asymptotic, "- MCE:", me_asymptotic, "\n")
  cat("Welch Rejection Ratio:", rejection_ratio_welch, "- MCE:", me_welch, "\n")
}


# NOTE: I am certain there is a simpler way to run the tests (likely using apply, replicate, etc.)
# However, I tried switching to that method and could not get it to work, so I built it using loops 
# which I am comfortable with. 
# If possible, I'd love to see what the solution looks like using the simpler functions like replicate/apply
# so I can get more used to using those





