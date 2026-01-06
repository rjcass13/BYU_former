# A function taking the following arguments:
#   log_density_target:   function(x) giving log(target density at x)
#   log_density_envelope: function(x) giving log(envelope density at x)
#   sample_envelope:      function() returning one sample from envelope
#   multiplier:           numeric constant such that
#                         multiplier * envelope(x) >= target(x) for all x
#   sample_size:          number of samples to draw
#
# The function returns `sample_size` realizations from the target distribution.

rejection_sampler <- function(log_density_target, log_density_envelope, sample_envelope, multiplier, sample_size) {
  out <- vector(mode = 'list', length = sample_size) # Preallocate storage
  count <- 0L
  trials <- 0L

  while (count < sample_size) {
    u <- runif(1)
    y <- sample_envelope(1)
    # Acceptance criteria
    if (log(u) <= log_density_target(y) - log_density_envelope(y) - log(multiplier)) {
      count <- count + 1L
      out[[count]] <- c(y[1], y[2])
    }
    trials <- trials + 1L
  }
  simplify2array(out)
}
