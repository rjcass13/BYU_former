library(boot)
set.seed(1337)

# data from: https://dahl.byu.edu/624/2025d/optimization/tied-up-normal-mle.R
samp <- c(3.08, 0.68, 2.09, 0.87, -0.02, 0.25, 1.98, 1.47, 1.95, 0.99)

find_mean_log <- function(data, indices){
  log(mean(data[indices]))
}
find_lower_log <- function(data, indices){
  mu <- mean(data[indices])
  sd <- sd(data[indices])
  n <- length(data)
  log(mu + qnorm(.025) * sd / sqrt(n))
}
find_upper_log <- function(data, indices){
  mu <- mean(data[indices])
  sd <- sd(data[indices])
  n <- length(data)
  log(mu + qnorm(.975) * sd / sqrt(n))
}

boot_value <- boot(data = samp, statistic = find_mean_log, R = 1000)

# BCA
boot_value <- boot(data = samp, statistic = find_mean_log, R = 1000)
ci_bca <- boot.ci(boot_value, conf = 0.95, type = 'bca')
mean_bca <- exp(ci_bca$t0)
lower_bca <- round(exp(ci_bca$bca[,4]), 4)
upper_bca <- round(exp(ci_bca$bca[,5]), 4)

# Percentile
boot_value <- boot(data = samp, statistic = find_mean_log, R = 1000)
ci_perc_mean <- boot.ci(boot_value, conf = 0.95, type = 'percentile')
mean_perc <- exp(ci_perc_mean$t0)
boot_value <- boot(data = samp, statistic = find_lower_log, R = 1000)
ci_perc_lower <- boot.ci(boot_value, conf = 0.95, type = 'percentile')
lower_perc <- round(exp(ci_perc_lower$t0), 4)
boot_value <- boot(data = samp, statistic = find_upper_log, R = 1000)
ci_perc_upper <- boot.ci(boot_value, conf = 0.95, type = 'percentile')
upper_perc <- round(exp(ci_perc_upper$t0), 4)


# Use both BCA and Percentile
cat("Percentile | Mean:", mean_perc, "Lower:", lower_perc, "Upper:", upper_perc, '\n')
cat("   BCA     | Mean:", mean_bca, "Lower:", lower_bca, "Upper:", upper_bca, '\n')