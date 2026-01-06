In order to perform maximum likelihood estimation on a Shared Variance distribution, I used the Newton-Raphson method. The ensure numerical stability, I generated initial parameter estimates in 'theta' space, then converted it to a log-space (eta) and performed Maximum Likelihood estimation on eta (using Newton-Raphson). Once the eta parameters were estimated, I then back-transformed them into theta-space.

In order to see the behavior of this process, I tried a variety of intitial parameters. Using quantile estimates for mu, and the sigma estimate from the data, I found that my initial estimates for pi had a very large impact on the outcome of the estimated parameters. For example, as I adjusted pi over the range of 0-1, I found that the root finding method tend towards 2 options:
1. Have both mu's equal to each other, and set the pi to ~.5
2. Have pi be very close to 0 or 1 and the mu for the small probabiliy would just be some seeminlgy random value
Both of these results shows the root finding method reducing the distirbution down to essentially 1 normal distribution

Through a large amount of guessing and checking with my initial pi and sigma2 estimates (including looking at the Histogram of the generated data to get a reasonable guess for pi), I found the following initial estimates to provide good convergence (with z as the generated data)
pi: .33
mu1: 25th percentile of z
mu2: 75th percentile of z
sigma2: .3 of the standard deviation of z

This gave the following eta estimates:
alpha: .693
mu1: -1.92
mu2: 2.24
tau: -0.05
as well as a Maximum Log-likelihood value of -196.4

And the following theta estimates:
pi: .667
mu1: -1.92
mu2: 2.24
sigma2: .95

In terms of performance of the Newton-Raphson, the given initial estimates resulted in a process that took 6 iterations, returned a final gradient of (-1.42 * 10^-9, -4.26 * 10^-9, 7.11 * 10^-9, 1.28 * 10^-8), and a final Hessian that IS negative definite. 
