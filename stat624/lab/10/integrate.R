set.seed(1337)
#######################
####### PART 1 ########
#######################
riemann_mid <- function(f, n, lower, upper) {
  diff <- (upper - lower)/n # Find width of all intervals
  x <- seq(lower + diff/2, upper - diff/2, length = n) # Find all midpoints
  fx <- f(x)
  sum(fx * diff)
}

# E(X | 0 <= X <= 2) = INT(x * fx) / INT(fx)
num <- riemann_mid(\(x) x * dnorm(x, sd = 1), 100000, 0, 2) 
den <- riemann_mid(\(x) dnorm(x, sd = 1), 100000, 0, 2)
num/den # .7227898

# Compare to MonteCarlo estimate
x <- rnorm(1e6)
mean(x[x > 0 & x < 2]) # .7217621

# The answers are very close, a difference of ~.001
# For day to day, not a big deal, but could cause issues in certain circumstances where tolerance is much tighter
# Also, the estimation method is frankly much simpler

#######################
####### PART 2 ########
#######################
# Integrand for part b
part_b_integrand = function(theta) {
  dnorm(1, mean = theta, sd = 1) * dnorm(theta, mean = 0, sd = 2)
}

integrate(part_b_integrand, -Inf, Inf) # 0.1614342 +/- 8.3e-05

# Analytic solution
dnorm(1, sd = sqrt(1+4)) # 0.1614342

# The two methods do match!
# I do like that the integrate function gives an error margin

#######################
####### PART 3 ########
#######################
fxy <- function(x, y) {
  exp(-3 * x^2 - 2 * y^2 - x * y)
}

# 2 Dimensional reimann-midpoint with uniform grid
# I made it so it can have un-equal bounds
# For this specific problem, I could have simplified it to just calculate grid once (both dims use same grid)
riemann_2d <- function(fxy, nx, ny, x_bounds, y_bounds) {
  # Find the distnace between each spot
  x_diff <- (x_bounds[2] - x_bounds[1])/nx
  y_diff <- (y_bounds[2] - y_bounds[1])/ny

  # Find the arrays of midpoints
  x_mids <- seq(x_bounds[1] + x_diff/2, x_bounds[2] - x_diff/2, length = nx)
  y_mids <- seq(y_bounds[1] + y_diff/2, y_bounds[2] - y_diff/2, length = ny)
  
  # Get all combos of the x_mids and y_mids
  grid <- expand.grid(x_mids, y_mids)

  # Get the sum of the volume of each prism at each combo of midpoints
  sum(mapply(fxy, grid[,1], grid[,2])) * x_diff * y_diff
}

# Parameters for integration
nx <- 40; ny <- 40; x_bounds <- c(-2, 2); y_bounds <- c(-2, 2)

# Estimte normalizing constant C
riemann_2d(fxy, nx, ny, x_bounds, y_bounds) # 1.310018

# MonteCarlo estimate
n = 2e5
x <- runif(n, -2, 2)
y <- runif(n, -2, 2)
mean(fxy(x, y)) * (4*4) # 1.303668

# The values are similar (~.067 difference).
# However, the Riemann method originally took ~30 lines (using for loops), and even once reducted, takes ~20
# Meanwhile, the MonteCarlo takes ~4 lines (excluding the fxy since they both used it)
# The MonteCarlo simpler to implement, but in particular just has less calculations to perform overall
# For example, Riemann method made nx*ny function calls, while MonteCarlo makes n
# If you wanted to make Riemann more accurate it scales by n^2, but MonteCarlo only by n
# If the function has sharp boundaries, Riemann will be less accurate meaning you'd need more buckets (increasing n)

# Also, in general, I will say I'm rather proud of how I implemented this lab
# I can see myself getting better at recognizing ways to reduce lines/code
# such as using anonymous functions, lapply, etc. Good learnings!