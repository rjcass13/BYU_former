# bisection.R
# Find a root of f in [a, b] assuming f(a) * f(b) < 0
bisection <- function(f, a, b, tol = 1e-8, maxit = 100L) {
  # Initialize
  iter = 1
  f_count = 0
  lower = a
  upper = b
  conditions_met = FALSE
  
  # While we are still out of range of 0, keep repeating the bisection
  while (conditions_met == FALSE) {
    mid = (upper + lower)/2
    fmid = f(mid)
    f_count = f_count + 1

    # Check if the midpoint is within tolerance range of 0
    if (abs(fmid) <= tol) {
      conditions_met = TRUE
      converged = TRUE
      break
    } else if ((upper - lower) <= tol) {# Check if upper-lower distance within minimum tolerance
      conditions_met = TRUE
      converged = TRUE
      break
    } else if (iter > maxit) {
      conditions_met = TRUE
      converged = FALSE
      break
    }

    # If mid is below 0, it's the new lower bound. Otherwise, it's the new upper
    if (f(upper) < 0) {
      if (fmid > 0) {
        lower <- mid
      } else {
        upper <- mid
      }
    } else {
      if (fmid < 0) {
        lower <- mid
      } else {
        upper <- mid
      }
    }
    f_count = f_count + 1

    iter = iter + 1
}

  list(root = mid, converged = converged, iter = iter, feval = f_count, deval = 0)
}
