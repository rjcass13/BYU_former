# secant.R
# Find a root of f using two initial guesses x0, x1 (no bracketing required)
secant <- function(f, x0, x1, tol = 1e-8, maxit = 100L) {
  # Initialize
  xn1 = x1
  xn0 = x0
  conditions_met = FALSE
  iter = 1
  f_count = 0

  while (conditions_met == FALSE) {
    # Initialize
    fxn1 = f(xn1)
    fxn0 = f(xn0)
    f_count = f_count + 2

    if (fxn1 == fxn0) {
      warning("Division by zero encountered. Returning NA.")
      return(NA) # Return NA if division by zero
    } else if (abs(fxn1 - fxn0) <= 10^-8) {
      warning("Very small divisor detected. Results may not be as expected.")
    }

    xn2 = xn1 - fxn1 * (xn1 - xn0) / (fxn1 - fxn0)

    # Check if step within tolerance
    if (abs(xn1 - xn0) <= tol) {
      converged = TRUE
      break
    } else if (abs(fxn1) <= tol) {
      converged = TRUE
      break
    } else if (iter > maxit) {
      converged = FALSE
      break
    }
    
    # Update the values (xn-1 takes values of xn, xn takes value of xn+1)
    xn0 = xn1
    xn1 = xn2
    iter = iter + 1
  }

  list(root = xn2, converged = converged, iter = iter, feval = f_count, deval = 0)
}
