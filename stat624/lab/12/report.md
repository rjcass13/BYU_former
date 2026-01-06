In this lab, we performed Markov chain Monte Carlo via Gibbs Sampling on a mixture model. We performed 10k iterations, with a burnin of 1k. Using this method, we found the following values for each parameter:

| Parameter | Mean | Med. | 2.5% Quant | 97.5% Quant |   ESS  | MCMC Lower | MCMC Upper |
|:---------:|:----:|:----:|:----------:|:-----------:|:------:|:----------:|:----------:|
| pi        | .664 | .665 | .568       | .756        | 7429.9 | -1.02      | 2.35       |
| mu1       | -1.92| -1.92| -2         | -1.82       | 4575.9 | -1.921     | -1.919     |
| mu2       | 2.24 | 2.24 | 2.08       | 2.39        | 5006.3 | 2.238      | 2.242      |     
| sigma2    | .928 | .915 | .695       | 1.23        | 6771.1 | .925       | .931       |

We performed a similar analysis in Lab 9 using maximum likelihood estimation, from which we found the parameter values to be as follows:
| Parameter | Mean |
|:---------:|:----:|
| pi        | .667 |
| mu1       | -1.92|
| mu2       | 2.24 |  
| sigma2    | .95  |

Comparing the two of them, the mean estimates are the exact same to the precision I calculated, but the mixture and variance estimates are slightly different. I'm guessing that the Markov chain method requires extra iterations to fully 'lock in' the spread of the distirbutions, where as the means are 'easier' to hone in on. 

Reviewing the trace plots, it does look like the burnin period is contained within the first 200 or so iterations, so if we were to repeat this we could probably reduce the burnin significantly. 

Reviewing the autotrace plots, the Lag gets within a good value within 3-4 iterations, indicating that the proposals and iteration steps are reasonable and do not have long-lasting correlations. 