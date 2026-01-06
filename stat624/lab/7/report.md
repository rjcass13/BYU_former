The formula I used for u_t is: (1 - (U + 1) * r^U + U * r^(U + 1)) / (rho * (1 - r^U))

Description of Solvers:
Bisection: This method takes the midpoint of two points and sees if the function at the midpoint is more or less than 0. Based on that, it sets the midpoint as one of the new end points and finds the new midpoint, etc.
Secant: This method approxiamates derivatives using the function evaluation of two points. Take small steps forward/backward using a function of the point values and their function values. 
Netwon: Similar to secant, but uses the actual derivative to take the small steps forward/backward. 

| U | mu | Method | Root | Iterations | Function Evals | Derivative Evals | Converged |
|:-:|:-:|:-:|:-:|:-:|:-:|:-:|:-:|
| 10 | 3 | Bisect | 0.3062821 | 26 | 51 | 0 | TRUE | 
| 10 | 3 | Secant | 0.3062821 | 8 | 16 | 0 | TRUE | 
| 10 | 3 | Newton | 0.3062821 | 6 | 18 | 6 | TRUE | 
| 20 | 5 | Bisect | 0.1881808 | 28 | 55 | 0 | TRUE | 
| 20 | 5 | Secant | 0.1881808 | 8 | 16 | 0 | TRUE | 
| 20 | 5 | Newton | 0.1881808  | 8 | 16 | 8 | TRUE | 
| 50 | 7 | Bisect | 0.1423871 | 28 | 55 | 0 | TRUE | 
| 50 | 7 | Secant | 0.1423871 | 14 | 28 | 0 | TRUE | 
| 50 | 7 | Newton | 0.1423871  | 7 | 21 | 7 | TRUE | 