#Requires dependencies (normalRV & chi2RV) from chi2RV.R
#Requires dependencies (chi2RW & chi2RWArray) from chi2RW.R

deg = 3 #Should not change.
count = 1e4
N = 1e4
bars = 20

start.time <- Sys.time()
walks <- chi2RWArray(deg, N, count)
walks = (walks-deg*N)/sqrt(2*deg*N) #Scale
hist(walks, prob=TRUE, xlab = paste0("U_",N), ylab = "Probability density", main=paste0("Probability histogram of scaled\n chi^2 random walks Z_",N),breaks=bars)
end.time <- Sys.time()
time.taken <- round(end.time - start.time,2)
time.taken
paste0("ScaledPlot,N=",N,",count=",count,",deg=",deg,",bars=",bars)
#Manual line
hist(walks, prob=TRUE, xlab = paste0("U_",N), ylim=c(0,0.4),ylab = "Probability density", main=paste0("Probability histogram of scaled\n chi^2 random walks Z_",N),breaks=100)
