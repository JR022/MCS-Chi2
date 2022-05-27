normalRV <- function(N) { # The function returns 2N values from a N(0,1) distribution.
  U1 <- runif(N) #Generate N values from Unif[0,1].  These represent U_1 in the transformation formula.
  U2 <- runif(N) #Generate another N values from Unif[0,1].  These represent U_2 in the transformation formula.
  
  Z0 <- sqrt(-2*log(U1))*cos(2*pi*U2)
  Z1 <- sqrt(-2*log(U1))*sin(2*pi*U2)
  
  return(c(Z0,Z1)) #Join the two arrays together to give the 2N values.
}

plot <- function(N,count,bars,x) {
  print("Starting.")
  start_time <- Sys.time()
  
  #Generate a number ('count' many) of N(0,1) points.
  Z = normalRV(count)
  
  #Plot the histogram of the artificial N(0,1) points.  The number of bars 'may be chosen'bars' is supplied as an argument.
  bins <- -bars:bars*x/bars
  hist(Z, bins, xlim=c(-x,x), ylim=c(0,0.45), prob=TRUE, xlab="t",ylab="Probability", main="")
  
  #Generate the N(0,1) density in [-x,x].  The scale N defines how precise the interval is split.
  xpoints = -N:N*x/N
  ypoints = 1/sqrt(2*pi)*exp(-0.5*xpoints*xpoints)
  lines(xpoints,ypoints)
  
  print(sprintf("Duration %f seconds.",difftime(Sys.time(), start_time, units = "secs")))
  return(1)
}