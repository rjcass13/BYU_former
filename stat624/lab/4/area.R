library(dplyr)
set.seed(1)

# Generate a random spot with the larger boundary and see if it falls within target boundary
# Returns TRUE/FALSE for whether or not it's within the designated area
check_point = function(x, y) {
  # Check (x,y)
  region1 = y < x

  # Check x^2 + y^2 <= .5
  region2 = x^2 + y^2 <= .5

  # Check x^2 <= y
  region3 = x^2 <= y
    
  # If all region requirements are met, return true
  region1 & region2 & region3
}

# Create a random sample of size n_samp points with boundary region
# Return the proportion of how many were within target region
generate_mean = function (x_min, x_max, y_min, y_max, n_samp) {
  x = runif(n_samp, x_min, x_max)
  y = runif(n_samp, y_min, y_max)

  mapply(check_point, x, y) |> mean()
}

# Esetimate the size of the target area given a boundary area
sample_area = function(n_samp, n_reps, x_min, x_max, y_min, y_max) {
  # Find the overall boundary area
  area = (x_max - x_min) * (y_max - y_min)

  # For one sample, pick n_samp points. Find the proportion of how many points fall in the area
  # Repeat that test n_reps time to find the average area proportion
  est_prop = generate_mean(x_min, x_max, y_min, y_max, n_samp) |> 
    replicate(n = n_reps) |> 
    mean()

  # Calculate the total area by multiplying the proportion of the area by the total boundary area
  est = est_prop * area

  # Get Monte Carlo Error
  me <- qnorm(0.975) * sqrt(est * (1 - est) / n_reps)

  # Get coverage range (including error)
  c(estimate = est, lower = est - me, upper = est + me)
}

# Set the outer boundary parameters
x_min = 0
x_max = 1
y_min = 0
y_max = 1
n_samp = 100
n_reps = 100000
print(sample_area(n_samp, n_reps, x_min, x_max, y_min, y_max))


