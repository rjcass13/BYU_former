# newton.R
# Find a root using Newton's method. You may set fprime to be an R function that
# evaluates the derivation, or leave it as NULL and you would then use a
# numerical derivative.
newton <- function(f, x0, fprime = NULL, tol = 1e-8, maxit = 100L) {
  # Initialize
  conditions_met = FALSE
  iter = 1
  f_count = 0
  deriv_count = 0

  while (conditions_met == FALSE) {
    # Initialize
    fx0 = f(x0)
    f_count = f_count + 1

    if (!is.null(fprime)){
      dfx0 = fprime(x0)
    } else {
      # Find numerical central difference derivative
      h = abs(x0) * 10^(-8)
      dfx0 = (f(x0 + h) - f(x0 - h))/(2*h)
      f_count = f_count + 2
    }

    deriv_count = deriv_count + 1
    
    if (dfx0 == 0) {
      warning("Division by zero encountered. Returning NA.")
      return(NA) # Return NA if division by zero
    } else if (abs(dfx0) <= 10^-8) {
      warning("Very small divisor detected. Results may not be as expected.")
      return(NA)
    }

    x1 = x0 - fx0/dfx0

    # Check if step within tolerance
    if (abs(x1 - x0) <= tol) {
      converged = TRUE
      break
    } else if (abs(fx0) <= tol) {
      converged = TRUE
      break
    } else if (iter > maxit) {
      converged = FALSE
      break
    }
    
    # Update the values (xn-1 takes values of xn, xn takes value of xn+1)
    x0 = x1
    iter = iter + 1
  }

  list(root = x0, converged = converged, iter = iter, feval = f_count, deval = deriv_count)
}
