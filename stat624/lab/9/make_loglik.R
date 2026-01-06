logsumexp <- function(a, b) {
  m <- pmax(a, b) # pmax for vectorized operation
  m + log(exp(a - m) + exp(b - m))
}

log_psi <- function(sigma, mu, z) {
  (-1/2 * log(2 * pi * sigma^2)) - ((z - mu)^2)/(2*sigma^2)
}

make_loglik <- function(z) {
  function(eta) {
    alpha <- eta[1]
    mu1 <- eta[2]
    mu2 <- eta[3]
    tau <- eta[4]

    p <- ifelse(alpha >= 0, 1/(1+exp(-alpha)), exp(alpha)/(1 + exp(alpha)))
    sigma2 <- exp(tau)

    a <- log(p) + log_psi(sigma2, mu1, z)
    b <- log(1-p) + log_psi(sigma2, mu2, z)

    ## eta = c(alpha, mu1, mu2, tau)
    ## Use the data z from the enclosing environment.
    ## Returns scalar log-likelihood value.
    sum(logsumexp(a, b))
  }
}


