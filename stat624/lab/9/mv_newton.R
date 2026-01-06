mv_newton <- function(fn, x0, tol_grad = 1e-6, tol_step = 1e-8, fd_eps = 1e-5, maxit = 100) {
  # Initialize
  curr <- x0
  dims <- length(x0)
  iter = 1
  conditions_met = FALSE
  converged = FALSE

  while (conditions_met == FALSE) {
    # Calculate the gradient
    grad <- numeric(dims)
    for (i in 1:dims) {
      plus <- curr
      plus[i] <- curr[i] + fd_eps
      minus <- curr
      minus[i] <- curr[i] - fd_eps
      grad[i] <- (fn(plus) - fn(minus)) / (2 * fd_eps)
    }

    # Calculate the Hessian
    hess <- matrix(nrow = dims, ncol = dims)
    for (i in 1:dims) {
      for (j in 1:dims) {
        if (i == j) {
          plus <- curr
          plus[i] <- curr[i] + fd_eps
          minus <- curr
          minus[i] <- curr[i] - fd_eps
          hess[i, j] <- (fn(plus) - 2 * fn(curr) + fn(minus)) / (fd_eps^2)
        }
        else {
          pp <- curr
          pp[i] <- curr[i] + fd_eps
          pp[j] <- curr[j] + fd_eps
          pm <- curr
          pm[i] <- curr[i] + fd_eps
          pm[j] <- curr[j] - fd_eps
          mp <- curr
          mp[i] <- curr[i] - fd_eps
          mp[j] <- curr[j] + fd_eps
          mm <- curr
          mm[i] <- curr[i] - fd_eps
          mm[j] <- curr[j] - fd_eps
          hess[i,j] <- (fn(pp) - fn(pm) -fn(mp) + fn(mm)) / (4 * fd_eps^2)
        }
      }
    }

    # Calculate next x step
    tryCatch(new <- curr - solve(hess) %*% grad,   
      error = function(e) {
        print('Could not invert matrix')
        return(list(estimate = curr, value = fn(curr), gradient = grad, hessian = hess, n_iterations = iter, converged = converged))
      }
    )

    # Check if conditions met to end
    if (norm(grad, type = "2") <= tol_grad) {
      converged <- TRUE
      break
    }
    if (norm(new - curr, type = "2") <= tol_step) {
      converged <- TRUE
      break
    }
    if (iter >= maxit) {
      break
    }

    curr <- new
    iter <- iter + 1
  }
  

  ## Return a list with these results
  # list(
  #   estimate = x_final,        # Final parameter vector
  #   value = fn(x_final),       # Final objective value
  #   gradient = grad_final,     # Final gradient vector
  #   hessian = H_final,         # Final Hessian matrix
  #   n_iterations = iter,       # Number of iterations used
  #   converged = TRUE/FALSE     # Did it converge?
  # )
  list(estimate = curr, value = fn(curr), gradient = grad, hessian = hess, n_iterations = iter, converged = converged)
}



# test_fn <- function(x) -(x[1]-1)^2 - (x[2]-2)^2
# result <- mv_newton(test_fn, c(2, 4))
# print(result)