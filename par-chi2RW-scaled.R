#Requires dependencies (normalRV & chi2RV) from chi2RV.R
#Requires dependencies (chi2RW & chi2RWArray) from chi2RW.R

library('doParallel')
library('parallel')

cores <- 1 #Depends on the machine being used.  Find with detectCores()
cl <- makeCluster(cores) # create a cluster
registerDoParallel(cl) # register the cluster

deg = 3 #Should not change.
count = 1e4 #If not a multiple of cores, some extra values are produced.
N = 1e4 #Walk length
bars = 50
## Code to generate sample and plot histogram ##
start.time <- Sys.time()
walks <- foreach (i = 1:cores, .combine = c) %dopar% {
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

## Sample mean and variance ##
mean <- mean(walks)
variance <- var(walks)
paste0("Mean = ",mean,", variance= ",variance)
