# Function to transform selected distribution (given an array)
f = function(x){
  sapply(x, function(x) { 1 - 2/pi * (1 - x)^2 * exp(x) / x * asin(sqrt(x)) })
}

###############
# Exploration #
###############
# Examine the test distribution
# Plot it to see what it looks like then find a distribution similar to it
# x = runif(10000)
# fx = f(x)
# plot(x, fx)

# Important features: Goes to -inf at 0, very very small the more x increases
# Want a weighting function similar to that
# This is similar to a negative beta function with a very large beta, and alpha 1
# I am choosing to use beta(1, 2) for my weighting distribution


# Define my weighting distribution
p = function(x) {
  dbeta(x, alpha, beta)
}

# Plotted to confirm it looks similar to f(x)
# alpha = 1
# beta = 2
# px = -1*dbeta(x, alpha, beta)
#plot(x, px) 


######################
# Actual Calculation #
######################
n_samp = 100
n_reps = 10000
alpha = 1
beta = 2

# Define function to run one sample
one_sample = function(n_reps, alpha, beta) {
    x = rbeta(n_reps, alpha, beta)
    fx = f(x)
    px = p(x)
    weights = fx/px
    mean(weights)
}

# Estimate function
estimate = function(n_samp, n_reps, alpha, beta){
  means = c()
  for (i in 1:n_samp) {
    means = append(means, one_sample(n_reps, alpha, beta))
  }

  est = mean(means)
  me = qnorm(0.975) * sqrt(var(means) / n_samp)
  c(estimate = est, lower = est - me, upper = est + me)
}

print(estimate(n_samp, n_reps, alpha, beta))
