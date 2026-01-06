##### NOTE: 
# I know I have a lot of functions here. I tried to make functions useable across multiple of the problem (such as the gen_intervals).
# There are likely things I could do to reduce that number further, but I feel like trying to generalize the functions any more might lose some readability


# Provided functions to get the a,b values
minimum_width <- function(n, alpha) {
  b <- function(a) qchisq(1 - (alpha - pchisq(a, n - 1)), n - 1)
  a <- optimize(
    \(a) 1 / a - 1 / b(a),
    lower = 0,
    upper = qchisq(alpha, n - 1)
  )$minimum
  c(a, b(a))
}

equal_tailed <- function(n, alpha) {
  qchisq(c(alpha / 2, 1 - alpha / 2), n - 1)
}

# Generate the intervals. Broke into separate function to use in different parts
# Returns by Index: 1 - Eq. Upper, 2 - Eq. Lower, 3 - Min. Upper, 4 - Min. Lower
gen_intervals <- function(n, eq, min, mu = 0, sigma2 = 1, alpha = .05, dist_type = 'Normal') {
  # Generate a random sample
  if (dist_type == 'Normal') {
    x <- rnorm(n, mean = mu, sd = sqrt(sigma2))
  } else {
    x <- rexp(n, sigma2)
  }
  var_x <- var(x)
  # Calculate the intervals
  eq_int <- (n-1) * var_x / eq
  min_int <- (n-1) * var_x / min
  c(eq_int, min_int)
}

# Creates one sample, creates CI for that one sample, and checks if the true mean is contained in the sample interval
gen_one_samp <- function(n, eq, min, mu = 0, sigma2 = 1, alpha = .05, dist_type = 'Normal', ret_type = 'Coverage') {
  # Get the interval upper and lower bounds
  if (dist_type == 'Normal') {
    ints <- gen_intervals(n, eq, min, mu = 0, sigma2, alpha, 'Normal')
  } else {
    ints <- gen_intervals(n, eq, min, mu = 0, sigma2, alpha, 'Exp')
  }
  eq_lower <- ints[2]
  eq_upper <- ints[1]
  min_lower <- ints[4]
  min_upper <- ints[3]
  if (ret_type == 'Coverage') {
    # See if true sigma2 is within intervals
    within_eq <- (sigma2 > eq_lower) & (sigma2 < eq_upper)
    within_min <- (sigma2 > min_lower) & (sigma2 < min_upper)
    return(c(within_eq, within_min))
  } else {
    # Calculate widths
    width_eq <- eq_upper - eq_lower
    width_min <- min_upper - min_lower
    return(c(width_eq/width_min))
  }
}

# Run B tests and get total proportion that contained the true sigma2
part_1_study <- function(B = 10000, n = 10, mu = 0, sigma2 = 1, alpha = .05, dist_type = 'Normal') {
  # Calculate the a, b values once as oppposed to on each interation (don't change with the data, only with n/alpha)
  eq <- equal_tailed(n, alpha)
  min <- minimum_width(n, alpha)
  # Replicate the sample B times
  res <- replicate(B, gen_one_samp(n, eq, min, mu, sigma2, alpha, dist_type, 'Coverage'))
  # Find coverage of each method
  eq_coverage <- mean(res[1, ])
  eq_cov_se <- qnorm(1-alpha/2) * sd(res[1, ]) / B
  min_coverage <- mean(res[2, ])
  min_cov_se <- qnorm(1-alpha/2) * sd(res[2, ]) / B
  # Return coverages
  c(eq_coverage, eq_cov_se, min_coverage, min_cov_se)
}

# Generates B samples of the width ratio. Returns the mean of the ratios, as well as the estimated interval
part_2_study <- function(B = 10000, n = 10, mu = 0, sigma2 = 1, alpha = .05) {
  # Calculate the a, b values once as oppposed to on each interation (don't change with the data, only with n/alpha)
  eq <- equal_tailed(n, alpha)
  min <- minimum_width(n, alpha)
  # Replicate the sample B times
  res <- replicate(B, gen_one_samp(n, eq, min, mu, sigma2, alpha, 'Normal', 'Width'))
  # Find mean and interval of widths
  w_ratio <- mean(res)
  se <- qnorm(1-alpha/2) * sd(res) / B
  # Return coverages
  c(w_ratio, se)
}

set.seed(1337)
alpha = .05
n = 10
sigma2 = 1
mu = 0
B = 100000

# PART 1
part_1 <- part_1_study(B, n, mu, sigma2, alpha, 'Normal')


# PART 2
part_2_1 <- part_2_study(B, n, mu, sigma2, alpha)
n = 25
part_2_2 <- part_2_study(B, n, mu, sigma2, alpha)

# PART 3
n = 10
part_3 <- part_1_study(B, n, mu, sigma2, alpha, 'Exp')


cat('Part 1 | Eq. Tail Coverage:', part_1[1], '+/-', part_1[2], '| Min. Width Coverage:', part_1[3], '+/-', part_1[4])
cat('Part 2 | n = 10 | W1/W2:', part_2_1[1], '+/-', part_2_1[2])
cat('Part 2 | n = 25 | W1/W2:', part_2_2[1], '+/-', part_2_2[2])
print("The Min-Width method is more advantageous when n is smaller. The larger n gets, the smaller the equal tailed width gets, the less advantage the min-width has")
cat('Part 3 | Eq. Tail Coverage:', part_3[1], '+/-', part_3[2], '| Min. Width Coverage:', part_3[3], '+/-', part_3[4])
print("The methods still work for exponential data, but provide much lower coverage, and much larger confidence intervals")


#| DBD:
#| Well done!
#| Your confidence intervals assessing Monte Carlo error are mistaken.  E.g., you wrote:
#|    eq_cov_se <- qnorm(1-alpha/2) * sd(res[1, ]) / sqrt(n)
#| but is should be:
#|    eq_cov_se <- qnorm(1-alpha/2) * sd(res[1, ]) / B
#| and that makes a big difference!
#| 99/100
