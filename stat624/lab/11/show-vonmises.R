library(akima)

show_vonmises <- function(n, mu, nu, kappa1, kappa2, lambda, show_contours = TRUE, use_sir = FALSE) {
  samps <- 0
  if (n == 0) {samps <- 200}
  n <- max(n, samps)
  
  # Get a random sample
  samp <- t(rvonmises(n, mu, nu, kappa1, kappa2, lambda, use_sir))

  # From the samples, generate the countour lines (only if countour wanted)
  if (show_contours) {
    d <- numeric(n)
    for (i in 1:n) {
      d[i] <- dvonmises(samp[i,], mu, nu, kappa1, kappa2, lambda)
    }
    
    # Create a regular grid
    x_grid <- seq(-pi, pi, length.out = 50)
    y_grid <- seq(-pi, pi, length.out = 50)
    
    # Interpolate Z values onto the grid
    interp_data <- interp(x = samp[,1], y = samp[,2], z = d, xo = x_grid, yo = y_grid, duplicate = 'mean')
    
    # Plot the countour lines
    contour(interp_data$x, interp_data$y, interp_data$z)
    # If the points should be included, plot them too
    if (samps == 0) {
      points(samp, col = 'red')
    }
  } else { # Plot just the points
    plot(samp)
  }
}