source("bisection.R")
source("secant.R")
source("newton.R")

make_h <- function(U = u, mu_target = mu) {
  function(rho) {
    r <- 1 - rho
    muT <- (1 - (U + 1) * r^U + U * r^(U + 1)) / (rho * (1 - r^U))
    muT - mu_target
  }
}

us <- c(10, 20, 50)
mus <- c(3, 5, 7)

for (i in 1:3) {
  u <- us[i]
  mu <- mus[i]
  fh <- make_h()
  bisect <- bisection(fh, .1, .9)
  seca <- secant(fh, .2, .8)
  newt <- newton(fh, .3)

  cat("U: ", u, '\n')
  cat("mu: ", mu, '\n')
  if (length(bisect) != 1) {
    print("Bisection")
    cat("Root: ", bisect$root, '\n')
    cat("Iterations: ", bisect$iter, '\n')
    cat("Function Evals: ", bisect$feval, '\n')
    cat("Conditons Met: ", bisect$converged, '\n')
  }
  if (length(seca) != 1) {
    print("Secant")
    cat("Root: ", seca$root, '\n')
    cat("Iterations: ", seca$iter, '\n')
    cat("Function Evals: ", seca$feval, '\n')
    cat("Conditons Met: ", seca$converged, '\n')
  }
  if (length(newt) != 1) {
    print("Netwon")
    cat("Root: ", newt$root, '\n')
    cat("Iterations: ", newt$iter, '\n')
    cat("Function Evals: ", newt$feval, '\n')
    cat("Conditons Met: ", newt$converged, '\n')
  }
}

