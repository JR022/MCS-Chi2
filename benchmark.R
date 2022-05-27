library('doParallel')
library('RcppZiggurat')
## Benchmark parameters##
count  = 1e4;
deg = 3;
N = 1e3;
## Parallel configuration ##
cores <- 6 #Depends on the machine being used.  Find with detectCores()
cl <- makeCluster(cores) # create a cluster
registerDoParallel(cl) # register the cluster

## Function definitions ##
#Marsaglia method
m_normalRV <- function(N) { # The function returns 2N values from a N(0,1) distribution.
  rv = rep(0,2*N);
  for (i in 1:N) {
    flag = TRUE;
    while (flag) {
      #Generate two Unif[0,1] values until s=x^2+y^2<1.
      xy = runif(2);
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

m_chi2RV <- function(deg, N) { #Generates N many \chi^2(deg) variables.
  rv = rep(0,N);
  for (i in 1:N) {
    #normalRV(k) generates 2k N(0,1) values, so k=deg/2 if deg is even and if deg is odd, k=ceil(deg/2) and ignore the last one. 
    norms <- m_normalRV(ceiling(deg/2));
    for (j in 1:deg) {
      rv[i] = rv[i] + norms[j]^2;
    }
  }
  return(rv);
}

m_chi2RW <- function(deg, N) {
  #Returns the value of the sum of N chi^2(deg) values.
  steps <- m_chi2RV(deg, N);
  return(sum(steps));
}

m_chi2RWArray <- function(deg, N, number) {
  #Generates 'number' chi random walks formed of N chi^2(deg) variables.
  walks <- rep(0,number);
  for (i in 1:number) {
    walks[i] <- m_chi2RW(deg, N);
  }
  return(walks);
}

#Ziggurat method
z_chi2RV <- function(deg, count) {
  rv = rep(0,count);
  for (i in 1:count) {
    norms <- zrnorm(deg);
    for (j in 1:deg) {
      rv[i] = rv[i] + norms[j]^2;
    }
  }
  return(rv);
}

z_chi2RW <- function(deg, N) {
  #Returns the value of the sum of N chi^2(deg) values.
  steps <- z_chi2RV(deg, N);
  return(sum(steps));
}

z_chi2RWArray <- function(deg, N, number) {
  #Generates 'number' chi^2 random walks formed of N chi^2(deg) variables.
  walks <- rep(0,number);
  for (i in 1:number) {
    walks[i] <- z_chi2RW(deg, N);
  }
  return(walks);
}

## Benchmark ##
#Marsaglia
start.time <- Sys.time()
walks <- foreach (i = 1:cores, .combine = c) %dopar% {
  walks <- m_chi2RWArray(deg, N, ceiling(count/cores))
}
walks = (walks-deg*N)/sqrt(2*deg*N) #Scale
end.time <- Sys.time()
time.taken <- round(end.time - start.time,2);

#Ziggurat
start.time <- Sys.time()
walks2 <- foreach (i = 1:cores, .combine = c, .packages = 'RcppZiggurat') %dopar% {
  walks2 <- z_chi2RWArray(deg, N, ceiling(count/cores))
}
walks2 = (walks2-deg*N)/sqrt(2*deg*N) #Scale
end.time <- Sys.time()
time.taken2 <- round(end.time - start.time,2);

paste0("Marsaglia with N=",N,",count=",count,",deg=",deg," took ",time.taken)
paste0("Ziggurat with N=",N,",count=",count,",deg=",deg," took ",time.taken2)