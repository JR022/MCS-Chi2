paretoRV <- function(shape, scale, N) {
  #Generates N Pareto(shape,scale) values.
  U <- runif(N)
  paretoVariables <- scale/(1-U)^(1/shape)
}


plot <- function(N,count,bars,x) {
  print("Starting.")
  start_time <- Sys.time()
  
  #Generate a number ('count' many) of Pareto points.
  shape <- 3
  scale <- 1
  Z = paretoRV(shape,scale,count)
  
  #Plot the histogram of the artificial Pareto points.  The number of bars may be chosen; 'bars' is supplied as an argument.
  bins <- c(seq(from=scale,to=x,by=(x-scale)/bars),1000)
  hist(Z, breaks=bins, xlim=c(scale,x), ylim=c(0,shape), prob=TRUE, xlab="t",ylab="Probability density", col="Grey", border="Black", main="Probability density histogram of generated Pareto\n points with the density function of Pareto(3,1)")
  #hist(Z, nclass=bars, xlim=c(0,6), prob=TRUE, xlab="t",ylab="Probability density", col="Grey", border="Black", main="Probability histogram of generated Pareto\n points with the density function of Pareto(a,b)")

  #Generate the Pareto density in [scale,x].  The scale N defines how finely the interval is split.
  xpoints = seq(from=scale,to=8,by=1/N)
  ypoints = shape*scale^shape/xpoints^(shape+1)
  lines(xpoints,ypoints,col="Red")
  
  print(sprintf("Duration %f seconds.",difftime(Sys.time(), start_time, units = "secs")))
  return(1)
}