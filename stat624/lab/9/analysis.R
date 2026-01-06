source("mv_newton.R")
source("make_loglik.R")
source("plot_mixture_fit.R")

# Get theta estimates from the eta estimates
thetarize <- function(eta_est) {
  theta_est <- eta_est

  # eta: alpha, mu1, mu2, tau
  alpha <- eta_est$estimate[1,1]
  tau <- eta_est$estimate[4, 1]

  # Adjust to theta space
  p <- ifelse(alpha >= 0, 1/(1+exp(-alpha)), exp(alpha)/(1 + exp(alpha)))
  sigma2 <- exp(tau)

  theta_est$estimate[1, 1] <- p
  theta_est$estimate[4, 1] <- sigma2

  theta_est$value <- exp(eta_est$value)

  # Return modified
  theta_est
}

# Generate initial estimates using Quantile method
quantile_init <- function(z, frac, pi_0) {
  # Generate Theta initials
  mu1_0 <- quantile(z, .25)
  mu2_0 <- quantile(z, .75)
  sigma2_0 <- sd(z) * frac

  # Generate Eta initials
  alpha_0 <- log(pi_0 / (1 - pi_0)) # logit transform
  tau_0 <- log(sigma2_0) # log transform
  c(alpha_0, mu1_0, mu2_0, tau_0) # eta
}

# Generate data
set.seed(6245)
n1 <- 65
n2 <- 35
z  <- c(rnorm(n1, mean = -1.9, sd = 1.0), rnorm(n2, mean =  2.2, sd = 1.0))
z  <- sample(z)  # Shuffle the data

# Conduct test
frac <- .3
pi_0 <- .33
eta_0 <- quantile_init(z, frac, pi_0)
result <- mv_newton(make_loglik(z), eta_0)
eta_est <- result
theta_est <- thetarize(eta_est)
plot_mixture_fit(z, theta_est$estimate)

# Check if Negative Definite
eigen(eta_est$hessian)$values

