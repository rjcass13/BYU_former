# A function taking the following arguments:
#   log_density_target:   function(x) giving log(target density at x)
#   log_density_proposal: function(x) giving log(proposal density at x)
#   sample_proposal:      function() returning one sample from proposal
#   sample_size:          number of samples to draw
#
# The function returns `sample_size` realizations from an importance
# resampling approximation of the target distribution.

importance_resampler <- function(log_density_target, log_density_proposal, sample_proposal, sample_size) {
  # Allocate storage
  w_star <- numeric(sample_size)
  x_star <- vector(mode = 'list', length = sample_size)

  # Generate a sample and calculate its weight
  # Note: I am doing this in a loop because I couldn't get my dvonmises to handle a vector
  for (i in 1:sample_size) {
    x <- sample_proposal(1)
    w_star[i] <- log_density_target(x) / log_density_proposal(x)
    x_star[[i]] <- x
  }
  
  # Calculate sample weights
  w_hat <- sample_size * w_star / (sum(w_star))

  # Sample the indices using the sample weights
  indices <- sample(seq(1:sample_size), sample_size, replace = TRUE, prob = w_hat)

  # Output the array of samples at the selected indices
  simplify2array(x_star[indices])
}