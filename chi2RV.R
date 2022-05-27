normalRV <- function(N) { # The function returns 2N values from a N(0,1) distribution.
  rv = numeric(2*N);
  for (i in 1:N) {
    flag = TRUE;
    while (flag) {
      #Generate two Unif[-1,1] values until s=x^2+y^2<1.
      xy = 2*runif(2)-1; #runif returns Unif[0,1] values, so 2*runif-1 produces Unif[-1,1].
      s = xy[1]^2+xy[2]^2; #Computing s here saves time later as it is re-used.
      if (s<1) {
        flag = FALSE;
      }
    }
    #Compute the normal values and add to the array.
    rv[2*i-1] = xy[1]*sqrt(-2*log(s)/s);
    rv[2*i] = xy[2]*sqrt(-2*log(s)/s);
  }
  return(rv)
}

chi2RV <- function(deg, N) { #Generates N many \chi^2(deg) variables.
  rv = numeric(N);
  for (i in 1:N) {
    #normalRV(k) generates 2k N(0,1) values, so k=deg/2 if deg is even and if deg is odd, k=ceil(deg/2) and ignore the last one. 
    norms <- normalRV(ceiling(deg/2));
    for (j in 1:deg) {
      rv[i] = rv[i] + norms[j]^2;
    }
  }
  return(rv);
}

plotChi2RV <- function(N,count,bars,x,deg) {
  print("Starting.")
  start_time <- Sys.time()
  
  #Generate a number ('count' many) of chi^2(deg) points.
  X = chi2RV(deg,count)
  
  #Plot the histogram of the artificial chi^2(deg) points.  The number of bars may be chosen; 'bars' is supplied as an argument.
  bins <- 0:bars*x/bars
  hist(x=X[X<x], breaks=bins, xlim=c(0,x), ylim=c(0,0.3), prob=TRUE, xlab="t",ylab="Probability density", main="")
  
  #Generate the chi^2(deg) density in [0,x].  The scale N defines how precise the interval is split.
  xpoints = seq(from=0,to=x,by=x/N)
  #ypoints = xpoints^(deg/2-1)*exp(-xpoints/2)/2^(deg/2)/gamma(deg/2);
  ypoints = dchisq(xpoints,deg);
  lines(xpoints,ypoints,col="Red")
  
  print(sprintf("Duration %f seconds.",difftime(Sys.time(), start_time, units = "secs")))
  return(X) #Return the array.
}

#chi <- plotChi2RV(1000,1e3,50,20,3)