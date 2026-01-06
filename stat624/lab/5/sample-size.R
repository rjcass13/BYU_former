sample_size <- function(p, width, alpha, n_samples, dist = "normal") {
  # Initialize loop
  width_met = FALSE
  sample_size = 2

  # Stop loop once the n_reps is found (width is met)
  while (width_met == FALSE) {
    # Make an empty list to populate sample means
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
    prob_within_width = mean(within_width)

    # If the calculated width if smaller than desired width, end the loop
    if (prob_within_width >= p) {
      width_met = TRUE
    } else {
      sample_size = sample_size + 1
    }
    }

  return(sample_size)
}

print(sample_size(.9, 1, .05, 100, "lognormal"))



