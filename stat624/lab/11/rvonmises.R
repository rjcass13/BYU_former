# The univariate vonMises
# Bracken shared this article with me which contians the deets:
# https://www.jstor.org/stable/2346732?origin=crossref&seq=1
# Density: Page 1
dvm <- function(theta, mu, kappa) {
  (2 * pi * besselI(kappa, 0))^(-1) * exp(kappa * cos(theta - mu))
}
# Sampling from univariate: page 4
rvm <- function(mu, kappa) {
  # Step 0
  tau <- 1 + (1+4*kappa^2)^(1/2)
  rho <- (tau - (2*tau)^(1/2))/(2*kappa)
  r <- (1+ rho^2)/(2 * rho)
  while (TRUE) {
    u1 <- runif(1)
    u2 <- runif(1)
    u3 <- runif(1)
    # Step 1
    z <- cos(pi * u1)
    f <- (1 + r * z) / (r + z)
    c <- kappa * (r - f)
    # Step 2
    if ((c * (2 - c)) > u2) {
      theta <- mu + sign(u3 - .5) * acos(f)
      return((theta + pi) %% (2 * pi) - pi)
    } else if ((log(c / u2) + 1) < c) { # Step 3
      next
    } else {# Step 4
      theta <- mu + sign(u3 - .5) * acos(f)
      return((theta + pi) %% (2 * pi) - pi)
    }
  }
}

rvonmises <- function(n, mu, nu, kappa1, kappa2, lambda, use_sir = FALSE) {
  # Define the target and envelope functions
  log_target <- function(x) {
    dvonmises(x, mu, nu, kappa1, kappa2, lambda, log = TRUE)
  }
  log_envelope <- function(x) {
    log(dvm(x[1], mu, kappa1)) + log(dvm(x[2], nu, kappa2))
  }
  envelope_sample <- function(n) {
    c(rvm(mu, kappa1), rvm(nu, kappa2))
  }
  

  # Determine the Multiplier
  # I also setup this section with the idea of implementing the optimized methods
  # Using bivariate normal in Case 1, mixture model in Case 2. But, couldn't figure
  # out how to implement that

  # Cases determined from Mardia etal, Appendix, Theorem 3
  if (kappa1 * kappa2 >= lambda^2) {# Unimodal case, Mardia Case 1
    # Envelope is a bivariate normal
    # Mode is at (0, 0), with saddle points at (0, pi), (pi, 0)
    #envelope <- mvrnorm()
    multiplier <- dvonmises(c(0, 0), mu, nu, kappa1, kappa2, lambda)

  } else if (lambda > 0) {# Bimodal case, lambda > 0, Mardia Case 2.1
    # Envelope is a mixture of normals
    # There are 2 modes, want to get the highest
    psi_0 <- abs(acos(kappa2 / abs(lambda) * sqrt((lambda^2 + kappa1^2)/(lambda^2 + kappa2^2))))
    phi_0 <- abs(acos(kappa1 / abs(lambda) * sqrt((lambda^2 + kappa2^2)/(lambda^2 + kappa1^2))))
    multiplier <- max(dvonmises(c(phi_0, psi_0), mu, nu, kappa1, kappa2, lambda), dvonmises(c(-phi_0, -psi_0), mu, nu, kappa1, kappa2, lambda))
  } else {# Bimodal case, lambda < 0, Mardia Case 2.2
    # Envelope is a mixture of normals
    # There are 2 modes, want to get the highest
    psi_0 <- abs(acos(kappa2 / abs(lambda) * sqrt((lambda^2 + kappa1^2)/(lambda^2 + kappa2^2))))
    phi_0 <- abs(acos(kappa1 / abs(lambda) * sqrt((lambda^2 + kappa2^2)/(lambda^2 + kappa1^2))))
    multiplier <- max(dvonmises(c(-phi_0, psi_0), mu, nu, kappa1, kappa2, lambda), dvonmises(c(phi_0, -psi_0), mu, nu, kappa1, kappa2, lambda))
  }
  if (use_sir) { # If they want to use the importance resampling
    importance_resampler(\(x) log_target(x), \(x) log_envelope(x), \(n) envelope_sample(n), n)
  } else { # Else use the rejection sampling
    rejection_sampler(\(x) log_target(x), \(x) log_envelope(x), \(n) envelope_sample(n), multiplier, n)
  } 
}
