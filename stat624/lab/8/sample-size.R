sample_size <- function(p, width, alpha, n_samples, dist = "normal") {

    
  p_diff <- function(sample_size){
    within_width = c()
    if (dist == "normal") {
      # Get n_reps sample means, determine if the interval width is within range
      for (i in 1:n_samples){
        sd = sd(rnorm(sample_size, 0, 1))
        interval_width = qt(1-alpha/2, sample_size) * sd / sqrt(sample_size) - qt(alpha/2, sample_size) * sd / sqrt(sample_size)
        within_width = append(within_width, interval_width <= width)
      }
    } else {
      # Log normal
      # Get n_reps sample means, determine if the interval width is within range
      for (i in 1:n_samples){
        sd = sd(rlnorm(sample_size, 0, 1))
        interval_width = exp(qt(1-alpha/2, sample_size)  * sd / sqrt(sample_size)) - exp(qt(alpha/2, sample_size) * sd / sqrt(sample_size))
        within_width = append(within_width, interval_width <= width)
      }
    }

    # Calculate the interval width of n_reps samples
    prob_within_width <- mean(within_width)
    p_diff <- p - prob_within_width
  }

  uniroot(p_diff, c(2, 1000))$root
}

# set.seed(1337)
# nreps <- 10
# n_calc <- numeric(nreps)
# for (i in 1:nreps) {
#   n_calc[i] <- sample_size(.9, .5, .05, 100, "normal")
# }
# n_mean <- mean(n_calc)
# n_sd <- sd(n_calc)
# lower <- n_mean - n_sd/nreps
# upper <- n_mean + n_sd/nreps
# cat("n:", n_mean, "lower:", lower, "upper:", upper)




  