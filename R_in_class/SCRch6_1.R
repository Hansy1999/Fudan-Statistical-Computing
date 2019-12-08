### Example 6.1 (Basic Monte Carlo estimation)

m <- 100000
g <- numeric(m)
for (i in 1:m) {
    x <- rnorm(2)
    g[i] <- abs(x[1] - x[2])
}
est <- mean(g)
est
# True mean
2/sqrt(pi)

# unbiased var
sd(g)/sqrt(m) # sd uses m-1 like var
# unbiased
sqrt(sum((g - mean(g))^2)/(m*(m-1))) 
# biased
sqrt(sum((g - mean(g))^2)) / m
# true var
sqrt((2-4/pi)/m)

### Example 6.2 (Estimating the MSE of a trimmed mean)
# trimming level 1
n <- 20
m <- 1000
tmean <- numeric(m)
for (i in 1:m) {
    x <- sort(rnorm(n))
    tmean[i] <- sum(x[2:(n-1)]) / (n-2)
    }
mse <- mean(tmean^2)
mse
sqrt(sum((tmean - mean(tmean))^2)) / m    #standard error

# median, trimming level maximized
n <- 20
m <- 1000
tmean <- numeric(m)
for (i in 1:m) {
    x <- sort(rnorm(n))
    tmean[i] <- median(x)
    }
mse <- mean(tmean^2)
mse
sqrt(sum((tmean - mean(tmean))^2)) / m    #se

# Different levels of trimming
n <- 20
m <- 10000 # try 10000
mse=numeric(10)

for(k in 0:9)
{
  tmean <- numeric(m) 
  for (i in 1:m) 
  {
    x <- sort(rnorm(n))
    tmean[i] <- sum(x[(k+1):(n-k)]) / (n-2*k) 
  }
  mse[k+1] <- mean(tmean^2)
}

plot(mse)

### Example 6.3 (MSE of a trimmed mean, cont.)

set.seed(522)
n <- 20
K <- n/2 - 1
m <- 10000
mse <- matrix(0, n/2, 6)

trimmed.mse <- function(n, m, k, p) {
    #MC est of mse for k-level trimmed mean of
    #contaminated normal pN(0,1) + (1-p)N(0,100)
    tmean <- numeric(m)
    for (i in 1:m) {
     sigma <- sample(c(1, 10), size = n,
         replace = TRUE, prob = c(p, 1-p))
     x <- sort(rnorm(n, 0, sigma))
     tmean[i] <- sum(x[(k+1):(n-k)]) / (n-2*k)
     }
    mse.est <- mean(tmean^2)
    se.mse <- sqrt(mean((tmean-mean(tmean))^2)) / sqrt(m)
    return(c(mse.est, se.mse))
}

for (k in 0:K) {
    mse[k+1, 1:2] <- trimmed.mse(n=n, m=m, k=k, p=1.0)
    mse[k+1, 3:4] <- trimmed.mse(n=n, m=m, k=k, p=.95)
    mse[k+1, 5:6] <- trimmed.mse(n=n, m=m, k=k, p=.9)
}

mse=data.frame(mse) # see errata
colnames(mse) = c('nMSE,p=1','nSE,p=1','nMSE,p=.95','nSE,p=.95','nMSE,p=.9','nSE,p=.9')
round(20*mse,digits = 3)
# balance between contamination level (p) and robustification level (k)
plot(mse[,5], type='o', col=4, lwd=2, ylim=c(0,.6), 
     xlab='Trimming level k', ylab='Mean Square Error', 
     main='Contamination v.s. Robustification')
lines(mse[,3], type='o',col=3,lwd=2)
lines(mse[,1], type='o',col=2,lwd=2)
legend("topright", legend = c("p=1","p=0.95","p=0.9"), 
       lwd=2, col = 2:4)

