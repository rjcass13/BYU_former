library('greybox')
library('coda')

mean_via_direct_sampling <- function(mu, sigma, B) {
  dt <- plogis(rnorm(n = B, mean = mu, sd = sigma))
  mean <- mean(dt)
  bounds <- mean + c(-1, 1) * 1.96 * sd(dt) / sqrt(B)
  c(mean, bounds[1], bounds[2])
}

# Helper function to only sample from the envelop inside the support of the logit-norm
# Honestly though, I got better results from a uniform
sample_in_support <- function(mu, sigma, B) {
  samp <- rep(NA, B)
  i <- 1
  # For normal cases, get the mean estimate using direct sampling
  sample_mean <- mean_via_direct_sampling(mu, sigma, 1000)
  while (TRUE) {
    x <- rnorm(1, mean = sample_mean, sd = sigma)
    if (x > 0 & x < 1) {
      # When mu is negative, distirbution is mirrored
      if (mu < 0) {x <- 1- x}
      samp[i] <- x
      i <- i + 1
      if (i > B) {break}
    }
  }
  samp
}

# This method struggles the most
mean_via_importance_sampling <- function(mu, sigma, B) {
  # I tested a bunch of different parameter values to try to figure out what would be a good value to use here
  # I tried to implement conditional envelopes depending on shape
  # However, it's not super great
  # Honestly, just a plain uniform for everything works as well if not better
  # But, I wanted to try to show some creativity on picking the envelope

  ##### MY ATTEMPT AT FANCY CONDITIONING FOR ENVELOPE
  # Use normal for anything where Sigma less than Mu
  if (sigma < abs(mu)) {
    x <- sample_in_support(mu, sigma, B)
    samp_mean <- mean_via_direct_sampling(mu, sigma, 1000)
    gx <- dnorm(x, samp_mean, sigma)
  # If sigma much larger than mu, use beta
  } else if (sigma >= abs(2*mu)) {
    scale <- ifelse(abs(mu) > 1, 1/abs(mu), abs(mu))
    x <- rbeta(1, scale, scale)
    gx <- dbeta(x, scale, scale)
  # Everything else use uniform
  } else {
    x <- samp <- runif(n = B, 0, 1)
    gx <- dunif(x, 0, 1)
  }
  
  
  fx <- dlogitnorm(x, mu, sigma)

  wx <- fx/gx
  hx <- plogitnorm(x, mu, sigma)

  mean <- plogis(sum(hx * wx) / B)
  se <- sqrt((1/(B * (B-1))) * sum(hx * wx - mean)^2)
  
  c(mean, mean - se, mean + se)
}


mean_via_mcmc <- function(mu, sigma, B) {
  # Using random samples form the full support so I don't need to figure out the different conditional probs
  # Also, beacuse random over same area, pi(x) is same for curr and next, and cancels
  # This ends up resulting in the sampler often getting stuck in the flat regions for extreme distirbutions
  # Results in it often giving values around .5 for those
  x <- rep(NA, B)
  curr <- runif(1)
  # Just picked a value of 1000. I know theoretically should look at those lag plots to get a real number. I just picked 1000
  burn_in <- 1000
  for (i in 1:(B+burn_in)) {
    proposed <- runif(1)
    f_curr <- dlogitnorm(curr, mu, sigma)
    f_prop <- dlogitnorm(proposed, mu, sigma)
    mh_ratio <- f_prop/f_curr
    if (i > burn_in) {x[i - burn_in] <- curr}
    if (runif(1) <= min(1, mh_ratio)) {
      curr <- proposed
    }
  }

  mean <- mean(x)
  ess <- coda::effectiveSize(x)[[1]]
  se <- sqrt(var(x) / ess)

  c(mean, mean - se, mean + se)
}


mean_via_quasi_monte_carlo <- function(mu, sigma, K) {
  # Just pulls directly from what's on Wikipedia
  x <- c(1:(K-1))
  1/(K-1) * sum(plogis(qnorm(x/K, mu, sigma)))
}

# Helper function for numerical integration
riemann_mid <- function(f, n, lower, upper) {
  diff <- (upper - lower)/n # Find width of all intervals
  x <- seq(lower + diff/2, upper - diff/2, length = n) # Find all midpoints
  fx <- f(x)
  sum(fx * diff)
}

mean_via_numerical_integration <- function(mu, sigma) {
  breaks <- 100000
  num <- riemann_mid(\(x) x * dlogitnorm(x, mu, sigma), breaks, 0, 1) 
  den <- riemann_mid(\(x) dlogitnorm(x, mu, sigma), breaks, 0, 1)
  num/den
}