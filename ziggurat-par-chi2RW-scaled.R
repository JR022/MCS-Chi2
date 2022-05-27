library('doParallel')
library('RcppZiggurat')

### Modified dependencies that use zrnorm ###
chi2RV <- function(deg, count) {
  rv = rep(0,count);
  for (i in 1:count) {
    norms <- zrnorm(deg);
    for (j in 1:deg) {
      rv[i] = rv[i] + norms[j]^2;
    }
  }
  return(rv);
}

chi2RW <- function(deg, N) {
  #Returns the value of the sum of N chi^2(deg) values.
  steps <- chi2RV(deg, N);
  return(sum(steps));
}

chi2RWArray <- function(deg, N, number) {
  #Generates 'number' chi^2 random walks formed of N chi^2(deg) variables.
  walks <- rep(0,number);
  for (i in 1:number) {
    walks[i] <- chi2RW(deg, N);
  }
  return(walks);
}

### Parallel configuration ###
cores <- 4 #Depends on the machine being used.  Find with detectCores()
cl <- makeCluster(cores) # create a cluster
registerDoParallel(cl) # register the cluster
### Variable configuration ###
deg = 3 #Should not change.
count = 50e3 #If not a multiple of cores, some extra values are produced.
N = 2e4
bars = 50

### Sample and plot ###
start.time <- Sys.time()
walks <- foreach (i = 1:cores, .combine = c, .packages = 'RcppZiggurat') %dopar% {
  walks <- chi2RWArray(deg, N, ceiling(count/cores))
}
walks = (walks-deg*N)/sqrt(2*deg*N) #Scale
hist(walks, prob=TRUE, xlab = paste0("U_",N), ylab = "Probability density", main=paste0("Probability histogram of scaled\n chi^2 random walks Z_",N),breaks=bars)
end.time <- Sys.time()
time.taken <- round(end.time - start.time,2)
time.taken
paste0("ScaledPlot,N=",N,",count=",count,",deg=",deg,",bars=",bars)

#Manual line
hist(walks, prob=TRUE, xlab = paste0("U_",N),xlim=c(-4,4), ylim=c(0,0.5),ylab = "Probability density", main=paste0("Probability histogram of scaled\n chi^2 random walks Z_",N),breaks=100)
lines(seq(-4,4,0.01),dnorm(seq(-4,4,0.01)),col='red')
stopCluster(cl)

#Non-parallel version
start.time <- Sys.time()
walks2 <- chi2RWArray(deg, N, count)
walks2 = (walks2-deg*N)/sqrt(2*deg*N) #Scale
hist(walks2, prob=TRUE, xlab = paste0("U_",N), ylab = "Probability density", main=paste0("Probability histogram of scaled\n chi^2 random walks Z_",N),breaks=bars)
end.time <- Sys.time()
time.taken <- round(end.time - start.time,2)
time.taken
paste0("ScaledPlot,N=",N,",count=",count,",deg=",deg,",bars=",bars)
#Manual line
hist(walks2, prob=TRUE, xlab = paste0("U_",N), ylim=c(0,0.4),ylab = "Probability density", main=paste0("Probability histogram of scaled\n chi^2 random walks Z_",N),breaks=100)
