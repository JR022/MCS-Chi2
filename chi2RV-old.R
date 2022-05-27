plot <- function(N,count,bars,x,deg) {
  print("Starting.")
  start_time <- Sys.time()
  
  #Generate a number ('count' many) of chi^2(deg) points.
  Z = chi2RV(deg,count)
  
  #Plot the histogram of the artificial chi^2(deg) points.  The number of bars may be chosen; 'bars' is supplied as an argument.
  bins <- 0:bars*x/bars
  hist(Z, bins, xlim=c(0,x), ylim=c(0,0.3), prob=TRUE, xlab="t",ylab="Probability", main="")
  # main="Probability histogram of generated normal\n points with the density function of N(0,1)"
  
  #Generate the chi^2(deg) density in [0,x].  The scale N defines how precise the interval is split.
  xpoints = seq(from=0,to=x,by=1/N)
  ypoints = xpoints^(deg/2-1)*exp(-xpoints/2)/2^(deg/2)/gamma(deg/2);
  lines(xpoints,ypoints,col="Red")
  
  print(sprintf("Duration %f seconds.",difftime(Sys.time(), start_time, units = "secs")))
  return(1)
}