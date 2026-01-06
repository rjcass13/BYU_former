# With the exception of the gamma function, all were 
# built directly using the sheet provided. The gamma required
# further searching online to figure out which parameter
# in the equation provided was supposed to represent
# shape/rate, etc. 

# rnorm - n, mean = 0, sd = 1
rnorm_rj = function(n, mean = 0, sd = 1) {
  u1 = runif(n, 0, 1)
  u2 = runif(n, 0, 1)

  mapply(function(u1, u2) mean + sd * sqrt(-2 * log10(u1)) * cos(2 * pi *u2), u1, u2)
}

# rlnorm - n, meanlog = 0, sdlog = 1
rlnorm_rj = function(n, meanlog = 0, sdlog = 1) {
  exp(rnorm_rj(n, meanlog, sdlog))
}

# rexp - n, rate = 1
rexp_rj = function(n, rate = 1) {
  x = runif(n, 0, 1)
  sapply(x, function(x) -1 * log10(x)/rate)
}

# rpois - n, lambda
rpois_rj = function(n, lambda) {
  # Creat empty list
  result = c()
  
  for (i in 1:n) {
    # For each element of x, see if the product of the first j elements is 
    # less than exp(-lambda). If so, return j-1
    value_found = FALSE
    x = c()
    j = 1
    while (value_found == FALSE) {
      x = append(x, runif(1))
      if (prod(x) < exp(-1 * lambda)) {
        result = append(result, j - 1)
        value_found = TRUE
      }
      j = j + 1
    }
  }  
  
  return(result)
}

# rgamma - n, shape, rate = 1, scale = 1/rate
rgamma_rj = function(n, shape, rate = 1, scale = 1/rate) {
  if (missing(rate)) {
    rate = 1/scale
  }

  result = c()

  for (i in 1:n) {
    u = runif(shape)
    x = (-1 / rate) * sum(sapply(u, log10))
    result = append(result, x)
  }

  return(result)
}

# rchisq - n, df, ncp = 0
rchisq_rj = function(n, df, ncp = 0) {
  rgamma_rj(n, df/2, 2)
}

# rt - n, df, ncp
rt_rj = function(n, df, ncp) {
  y = rnorm_rj(n)
  z = rchisq_rj(n, df)
  
  mapply(function(y,z) y/sqrt(z/df), y, z)
}

# rf - n, df1, df2, ncp
rf_rj = function(n, df1, df2, ncp) {

  z = rchisq_rj(n, df1)
  w = rchisq_rj(n, df2)
  
  mapply(function(z,w) (z/df1)*(w/df2), y, z)
}

# rbeta - n, shape1, shape2, ncp = 0
rbeta_rj = function(n, shape1, shape2) {
  y = rgamma_rj(n, shape1, 1)
  z = rgamma_rj(n, shape2, 1)

  mapply(function(y,z) y/(y+z), y, z)
}

# rbinom - n, size, prob
rbinom_rj = function(n, size, prob) {
  bernoulli = function(p) {
    u = runif(1)
    return(u < p)
  }

  results = c()
  for (i in 1:n) {
    x = bernoulli(prob) |> replicate(n = size) |> sum()
    results = append(results, x)
  }
  
  return(results)
}


# Couldn't quite wrap my head around the setup for this one. Got to the point
# where I just ran with what I had. I think this is the one I'm supposed to use the
# inverse function stuff on, but couldn't figure it out
# rnbinom - n, size, prob, mu
rnbinom_rj = function(n, size, prob = .5, mu = 0) {
  if (missing(prob)) {
    prob = size / (size + mu)
  }
  u = runif(size)

  results = c()
  for (i in 1:n) {
    results = append(results, sapply(u, function(u) {log10(u)/log10(1-prob)}))
  }

  return(results)
}


#result = rnorm_rj(10000, 0, 1)
#result = rlnorm_rj(10000, 0, 1)
#result = rexp_rj(10000, .2)
#result = rpois_rj(1000, 5)
#result = rgamma_rj(10000, 8, 1)
#result = rchisq_rj(10000, 5)
#result = rt_rj(10000, 5)
#result = rf_rj(10000, 5, 3)
#result = rbeta_rj(10000, 5, 3)
#result = rbinom_rj(10000, 10, .6)
result = rnbinom_rj(10000, 3, .6, 3)
hist(result)



