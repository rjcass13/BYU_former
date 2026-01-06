##### NOTE:
# My code generates a fair number of errors when doing the boostrapping due to alpha, beta < 0. 

# Beta function in germs of gammas
Beta <- function(alpha, beta) {
  gamma(alpha) * gamma(beta) / gamma(alpha + beta)
}

# Calculates the fx value given the data X in terms of alpha and beta
fx <- function(x) {
  function(eta) {
    alpha <- eta[1]
    beta <- eta[2]
    mean(x^(alpha - 1) * (1-x)^(beta - 1) / Beta(alpha, beta))
  }
}

# Initial values
d <- c(0.66, 0.02, 0.07, 0.88, 0.88, 0.48, 0.50, 0.01, 0.27, 0.13, 0.45, 0.79, 0.42, 0.61, 0.76, 0.53, 0.16, 0.46, 0.61, 0.43, 0.50, 0.77, 0.57, 0.21, 0.51, 0.55, 0.14, 0.08, 0.09, 0.73)

# Returns an estimated value for theta_hat = alpha/beta
# eta is a vector of [alpha, beta]
gen_estimate <- function(data, indices) {
  eta_0 <- c(10, 20)
  eta_hat <- optim(eta_0, fx(data[indices]))$par
  theta_hat <- eta_hat[1]/eta_hat[2]
  theta_hat
}



library(boot)
set.seed(1337)

# BCA
boot_value <- boot(data = d, statistic = gen_estimate, R = 1000)
ci_bca <- boot.ci(boot_value, conf = 0.95, type = 'bca')
mean_bca <- round(ci_bca$t0, 4)
lower_bca <- round(ci_bca$bca[,4], 4)
upper_bca <- round(ci_bca$bca[,5], 4)
print("I created a confidence interval for theta = alpha/beta by performing a BCA bootstrap. I used BCA because it only requires performing the bootstrap once, making it execute faster than the alternatives (also, takes less code).")
print("I tested larger numbers of bootstrap samples, but the interval didn't improve much.")
print("The upper bound depended a lot on my initial estimates for alpha, beta. Better initials gave lower upper bound.")
cat("BCA | MLE:", mean_bca, "Lower:", lower_bca, "Upper:", upper_bca, '\n')

#| DBD:
#| Good start, but your log likelihood isn't correct.
#| Thanks for your email about you recognizing that a transformation would be helpful.
#| Good job with the bootstrap.
#| 47/50
