### Normal distribution test for scaled random walk ###
#Requires a sample 'walks' of scaled random walks to be generated using chi2RW-scaled code.

### Anderson-Darling test ###
library('DescTools')
AndersonDarlingTest(chi, null = "pchisq", df=3, nullname="Chi-square distribution with 3 degrees of freedom")

### Kolmorgorov-Smirnov test ###
library('dgof')
ks.test(x, y, ..., alternative = c("two.sided", "less", "greater"), exact = NULL, tol=1e-8, simulate.p.value=FALSE, B=2000)

### Shapiro-Wilk test ###
sample <- rnorm(1000, mean = 5, sd = 3)
shapiro.test(walks) #Only sets of size 3 to 5000

### Jarque-Bera test ###
library('tseries')
jarque.bera.test(walks)


### Chi-square test for generation of values ###
#Requires a sample 'chi' of values generated using chi2RV code

### Cramér-von Mises test ###
library('goftest')
cvm.test(chi, null = "pchisq", df=3, estimated=FALSE, nullname="Chi-square distribution with 3 degrees of freedom")
