# MCS-Chi2
Code used to generate chi-squared random values by generating standard normal values with the Box-Muller method.

Written in R and used for my Monte Carlo Simulations module.  To speed up the simulations, parallel processing is utilised via the 'doParallel' library and the Ziggurat method is used instead of Box-Muller via the 'RcppZiggurat' library.
