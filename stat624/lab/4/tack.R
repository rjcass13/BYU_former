library(dplyr)
set.seed(1)

# Daughter's 95% confidence interval
int_lower = .23
int_upper = .49

# For nreps times, generate a sample (from population p = p_pop) of n_samp flips and calculate phat
# See if sampled phat falls within the daughter's confidence interval
# Get an overall proportion of phats which fall or don't fall in the range
coverage = function(p_pop, n_samp, n_reps, int_lower, int_upper) {
  est = rbinom(n_samp, 1, p_pop) |> 
    mean() |> 
    between(left = int_lower, right = int_upper) |> 
    replicate(n = n_reps) |> 
    mean()
  
  # Get Monte Carlo Error
  me <- qnorm(0.975) * sqrt(est * (1 - est) / n_reps)

  # Get coverage range (including error)
  c(estimate = est, lower = est - me, upper = est + me)
}

p_pop = .4
n_samp = 50
n_reps = 100000
print(coverage(p_pop, n_samp, n_reps, int_lower, int_upper))
