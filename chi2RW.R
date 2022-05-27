#Requires dependencies (normalRV & chi2RV) from chi2RV.R

chi2RW <- function(deg, N) {
  #Returns the value of the sum of N chi^2(deg) values.
  steps <- chi2RV(deg, N);
  return(sum(steps));
}

chi2RWArray <- function(deg, N, number) {
  #Generates 'number' chi random walks formed of N chi^2(deg) variables.
  walks <- numeric(number);
  for (i in 1:number) {
    walks[i] <- chi2RW(deg, N);
  }
  return(walks);
}

plotChi2RW <- function(N,walkLength,count,bars,x,deg) {
  print("Starting.")
  start_time <- Sys.time()
  
  #Generate a 'count' many chi random walks.
  X = chi2RWArray(deg, walkLength, count);
  
  #Create the histogram of the artificial chi^2(deg) points.  The number of bars may be chosen; 'bars' is supplied as an argument.
  bins <- 0:bars*x/bars
  h <- hist(x=X[X<x], breaks=bins, xlim=c(0,x), ylim=c(0,0.2), prob=TRUE, xlab="t",ylab="Probability", main="Probability histogram of\n generated chi^2(3) values")
  h$counts <- cumsum(h$counts)/sum(h$counts) # Replace the cell freq.s by cumulative freq.s .
  plot(h, main="",xlab="t",ylab="Cumulative probability") # Plot the cumulative probability histogram.
  
  #Generate the chi^2(deg) cdf in [0,x].  The scale N defines how precise the interval is split.
  xpoints = seq(from=0,to=x,by=1/N)
  ypoints = pchisq(xpoints,walkLength*deg);
  lines(xpoints,ypoints,col="Red")
  
  print(sprintf("Duration %f seconds.",difftime(Sys.time(), start_time, units = "secs")))
  return(X); #The array of walk values is returned.  This enables it to be used again with different plotting parameters.
}

plotChi2Array <- function(X, bars, N, dist) {
  h <- hist(x=X, nclass=bars, ylim=c(0,0.2), prob=TRUE, xlab="t",ylab="Probability", main="")
  h$counts <- cumsum(h$counts)/sum(h$counts) #Convert counts to cumulative.
  plot(h, main="",xlab="t",ylab="Cumulative probability") # Plot the cumulative probability histogram.
  
  #Generate the chi^2(deg) cdf in [0,x].  The scale N defines how precise the interval is split.
  xpoints = seq(from=0,to=100,by=1/N)
  ypoints = pchisq(xpoints,dist);
  lines(xpoints,ypoints,col="Red")
}
# walks <- plotChi2RW(1e2,10,1e5,100,70,3)

# h <- hist(x=walks[walks<80], breaks=, xlim=c(0,x), ylim=c(0,0.2), prob=TRUE, xlab="t",ylab="Probability", main="Probability histogram of\n generated chi^2(3) values")