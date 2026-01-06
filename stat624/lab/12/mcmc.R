library(coda)
library(plotMCMC)

rtruncnorm1 <- function(mean, sd, lower = -Inf, upper = Inf) {
  stopifnot(sd > 0, lower < upper)
  alpha <- (lower - mean) / sd
  beta  <- (upper - mean) / sd
  Phi_a <- if (is.finite(alpha)) pnorm(alpha) else 0
  Phi_b <- if (is.finite(beta))  pnorm(beta)  else 1
  u <- runif(1, min = 1e-12, max = 1 - 1e-12)
  q <- Phi_a + u * (Phi_b - Phi_a)
  mean + sd * qnorm(q)
}

log_dnorm <- function(x, mean, sd) {
  -0.5 * ((x - mean) / sd)^2 - log(sd) - 0.5 * log(2 * pi)
}

mixture2_gibbs <- function(z, B = 10000, burnin = 1000, prior, seed) {
  set.seed(seed)
  a <- prior[1]
  b <- prior[2]
  v1 <- prior[3]
  v2 <- prior[4]
  m1 <- prior[5]
  m2 <- prior[6]
  alpha0 <- prior[7]
  beta0 <- prior[8]
  n <- length(z)

  samp <- matrix(0, nrow = B, ncol = 4, dimnames = list(NULL, c('pi', 'mu1', 'mu2', 'sigma2')))
  p <- .5
  mu1 <- quantile(z, .25)
  mu2 <- quantile(z, .75)
  sigma2 <- var(z)
  samp[1, ] <- c(p, mu1, mu2, sigma2)
  
  for (i in 2:B) {
    sigma <- sqrt(sigma2)

    # Determine S vector
    log_w_i1 <- log(p) + log_dnorm(z, mu1, sigma)
    log_w_i2 <- log(1-p) + log_dnorm(z, mu2, sigma)
    df_w_i <- data.frame(col1 = log_w_i1, col2 = log_w_i2)
    m_i <- apply(df_w_i, 1, max)
    p_i1 <- exp(log_w_i1 - m_i) / (exp(log_w_i1 - m_i) + exp(log_w_i2 - m_i))
    p_i2 <- 1 - p_i1
    df_p_i <- data.frame(col1 = p_i1, col2 = p_i2)
    s <- apply(df_p_i, 1, \(p) sample(c(1, 2), 1, prob = p))

    # Determine n1 and n2 
    n1 <- sum(s == 1)
    n2 <- sum(s == 2)

    # Determine next PI step
    p <- rbeta(1, a + n1, b + n2)

    # Parameters for MUs
    V1 <- ((n1 / sigma2) + (1 / v1)) ^ (-1)
    z1 <- sum(z[which(s == 1)])
    m1_star <- V1 * ((z1 / sigma2) + (m1 / v1))
    V2 <- ((n2 / sigma2) + (1 / v2)) ^ (-1)
    z2 <- sum(z[which(s == 2)])
    m2_star <- V2 * ((z2 / sigma2) + (m2 / v2))

    mu1_new <- rtruncnorm1(m1_star, V1, -Inf, mu2)
    mu2 <- rtruncnorm1(m2_star, V2, mu1, Inf)
    mu1 <- mu1_new

    # Parameters for Sigma
    mus <- s
    mus[s == 1] <- mu1
    mus[s == 2] <- mu2
    SSE <- sum((z - mus)^2)
    sigma2 <- 1 / rgamma(1, shape = alpha0 + n/2, rate = beta0 + SSE/2)
    
    samp[i, ] <- c(p, mu1, mu2, sigma2)
  }

  samp[-(1:burnin), ]
}

mc_se_ess <- function(x) {
  ess <- as.numeric(coda::effectiveSize(x))
  sd(x) / sqrt(ess)
}

set.seed(6245)
n1 <- 65
n2 <- 35
z  <- c(rnorm(n1, mean = -1.9, sd = 1.0),
        rnorm(n2, mean =  2.2, sd = 1.0))
z  <- sample(z)  # Shuffle the data

B <- 10000
burnin <- 0
prior <- c(1, 1, 100, 100, median(z) - 1, median(z) + 1, 2, 2) # a, b, v1, v2, m1, m2, alpha0, beta0
res <- mixture2_gibbs(z, B, burnin, prior, 1337)

# pi intervals
pi_bar <- mean(res[, 1])
pi_med <- median(res[, 1])
pi_int <- c(quantile(res[, 1], .025), quantile(res[, 1], .975))
pi_int_mc <- pi_bar + c(-1, 1) * 1.96 * mc_se_ess(res[1, ])
pi_ess <- as.numeric(coda::effectiveSize(res[, 1]))

# mu1 intervals
mu1_bar <- mean(res[, 2])
mu1_med <- median(res[, 2])
mu1_int <- c(quantile(res[, 2], .025), quantile(res[, 2], .975))
mu1_int_mc <- mu1_bar + c(-1, 1) * 1.96 * mc_se_ess(res[, 2])
mu1_ess <- as.numeric(coda::effectiveSize(res[, 2]))

# mu2 intervals
mu2_bar <- mean(res[, 3])
mu2_med <- median(res[, 3])
mu2_int <- c(quantile(res[, 3], .025), quantile(res[, 3], .975))
mu2_int_mc <- mu2_bar + c(-1, 1) * 1.96 * mc_se_ess(res[, 3])
mu2_ess <- as.numeric(coda::effectiveSize(res[, 3]))

# sigma2 intervals
sigma2_bar <- mean(res[, 4])
sigma2_med <- median(res[, 4])
sigma2_int <- c(quantile(res[, 4], .025), quantile(res[, 4], .975))
sigma2_int_mc <- sigma2_bar + c(-1, 1) * 1.96 * mc_se_ess(res[, 4])
sigma2_ess <- as.numeric(coda::effectiveSize(res[, 4]))

x <- 1:(B-1)
plot(x, res[ , 4])

plotAuto(res[ ,4])

