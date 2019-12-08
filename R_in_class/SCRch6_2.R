### Example 6.4 (Confidence interval for variance)
n <- 20
alpha <- .05
x <- rnorm(n, mean=0, sd=2)
UCL <- (n-1) * var(x) / qchisq(alpha, df=n-1)
UCL

### Example 6.5 (MC estimate of confidence level)
#set.seed(123)
n <- 20
alpha <- .05
UCL <- replicate(1000, expr = {
    x <- rnorm(n, mean = 0, sd = 2)
    (n-1) * var(x) / qchisq(alpha, df = n-1)
} )
#count the number of intervals that contain sigma^2=4
sum(UCL > 4)
#or compute the mean to get the confidence level
a = mean(UCL > 4)
a
# standard error
sqrt(a*(1-a)/1000)

### Example 6.6 (Empirical confidence level)
# non-Gaussian case, chisquare(2)
n <- 20
alpha <- .05
UCL <- replicate(1000, expr = {
    x <- rchisq(n, df = 2)
    (n-1) * var(x) / qchisq(alpha, df = n-1)
} )
sum(UCL > 4)
a = mean(UCL > 4)
a
# standard error
sqrt(a*(1-a)/1000)

### Example 6.7 (Empirical Type I error rate)
# t-test for mean with unknown sigma
n <- 20
alpha <- .05
mu0 <- 500
sigma <- 100

m <- 10000          #number of replicates
p <- numeric(m)     #storage for p-values
for (j in 1:m) {
  x <- rnorm(n, mu0, sigma)
  ttest <- t.test(x, alternative = "greater", mu = mu0)
  p[j] <- ttest$p.value
}

p.hat <- mean(p < alpha)
se.hat <- sqrt(p.hat * (1 - p.hat) / m)
print(c(p.hat, se.hat))


### Example 6.8 (Skewness test of normality)

n <- c(10, 20, 30, 50, 100, 500) #sample sizes
cv <- qnorm(.975, 0, sqrt(6/n))  #crit. values for each n

sk <- function(x) {
  #computes the sample skewness coeff.
  xbar <- mean(x)
  m3 <- mean((x - xbar)^3)
  m2 <- mean((x - xbar)^2)
  return( m3 / m2^1.5 )
}

#n is a vector of sample sizes
#we are doing length(n) different simulations

p.reject <- numeric(length(n)) #to store sim. results
m <- 10000                     #num. repl. each sim.

for (i in 1:length(n)) {
  sktests <- numeric(m)       #test decisions
  for (j in 1:m) {
    x <- rnorm(n[i])
    #test decision is 1 (reject) or 0
    sktests[j] <- as.integer(abs(sk(x)) >= cv[i] )
  }
  p.reject[i] <- mean(sktests) #proportion rejected
}

p.asym = p.reject
plot(p.asym, ylim=c(0,.1), type='o', lwd=2)
abline(h=0.05, lty=2, lwd=2, col='red')

# Use an exact variance to calculate the critical values
cv <- qnorm(.975, 0, sqrt(6*(n-2) / ((n+1)*(n+3))))
#n is a vector of sample sizes
#we are doing length(n) different simulations

p.reject <- numeric(length(n)) #to store sim. results
m <- 10000                     #num. repl. each sim.

for (i in 1:length(n)) {
  sktests <- numeric(m)       #test decisions
  for (j in 1:m) {
    x <- rnorm(n[i])
    #test decision is 1 (reject) or 0
    sktests[j] <- as.integer(abs(sk(x)) >= cv[i] )
  }
  p.reject[i] <- mean(sktests) #proportion rejected
}

p.reject
lines(p.reject, lwd=2, type='b', col='blue')

### Example 6.9 (Empirical power)

n <- 20
m <- 1000
mu0 <- 500
sigma <- 100
mu <- c(seq(450, 650, 10))  #alternatives
M <- length(mu)
power <- numeric(M)
for (i in 1:M) {
  mu1 <- mu[i]
  pvalues <- replicate(m, expr = {
    #simulate under alternative mu1
    x <- rnorm(n, mean = mu1, sd = sigma)
    ttest <- t.test(x,
                    alternative = "greater", mu = mu0)
    # try "two.sided" and "less"
    ttest$p.value  } )
  power[i] <- mean(pvalues <= .05)
}

#par(ask = TRUE)
library(Hmisc)  #for errbar
plot(mu, power)
abline(v = mu0, lty = 1)
abline(h = .05, lty = 1)

#add standard errors
se <- sqrt(power * (1-power) / m)
errbar(mu, power, yplus = power+se, yminus = power-se,
       xlab = bquote(theta))
abline(v = mu0, lty = 1)
abline(h = .05, lty = 1)
lines(mu, power, lty=3)
detach(package:Hmisc)
#par(ask = FALSE)


### Example 6.10 (Power of the skewness test of normality)

alpha <- .1
n <- 30
m <- 2500
epsilon <- c(seq(0, .15, .01), seq(.15, 1, .05))
N <- length(epsilon)
pwr <- numeric(N)
#critical value for the skewness test
cv <- qnorm(1-alpha/2, 0, sqrt(6*(n-2) / ((n+1)*(n+3))))

for (j in 1:N) {           #for each epsilon
  e <- epsilon[j]
  sktests <- numeric(m)
  for (i in 1:m) {       #for each replicate
    sigma <- sample(c(1, 10), replace = TRUE,
                    size = n, prob = c(1-e, e))
    x <- rnorm(n, 0, sigma)
    sktests[i] <- as.integer(abs(sk(x)) >= cv)
  }
  pwr[j] <- mean(sktests)
}
#plot power vs epsilon
plot(epsilon, pwr, type = "b",
     xlab = bquote(epsilon), ylim = c(0,1))
abline(h = .1, lty = 3)
se <- sqrt(pwr * (1-pwr) / m)  #add standard errors
lines(epsilon, pwr+se, lty = 3, lwd=2, col=2)
lines(epsilon, pwr-se, lty = 3, lwd=2, col=2)


### Example 6.11 (Power comparison of tests of normality)

#only one loop, for epsilon=0.1, was shown in the text
#the simulation below takes several minutes to run

# initialize input and output
library(energy)
alpha <- .1
n <- 30
m <- 100        #try small m for a trial run
test1 <- test2 <- test3 <- numeric(m)

#critical value for the skewness test
cv <- qnorm(1-alpha/2, 0, sqrt(6*(n-2) / ((n+1)*(n+3))))
sim <- matrix(0, 11, 4)

# estimate power
for (i in 0:10) {
  epsilon <- i * .1
  for (j in 1:m) {
    e <- epsilon
    sigma <- sample(c(1, 10), replace = TRUE,
                    size = n, prob = c(1-e, e))
    x <- rnorm(n, 0, sigma)
    test1[j] <- as.integer(abs(sk(x)) >= cv)
    test2[j] <- as.integer(
      shapiro.test(x)$p.value <= alpha)
    test3[j] <- as.integer(
      mvnorm.etest(x, R=200)$p.value <= alpha)
  }
  print(c(epsilon, mean(test1), mean(test2), mean(test3)))  
  sim[i+1, ] <- c(epsilon, mean(test1), mean(test2), mean(test3))
}    
detach(package:energy)

# plot the empirical estimates of power
plot(sim[,1], sim[,2], ylim = c(0, 1), type = "l",
     xlab = bquote(epsilon), ylab = "power")
lines(sim[,1], sim[,3], lty = 2, col=2)
lines(sim[,1], sim[,4], lty = 4, col=3)
abline(h = alpha, lty = 3)
legend("topright", 1, c("skewness", "S-W", "energy"),
       lty = c(1,2,4), col=1:3, inset = .02)






###################################################
### Example 6.12 (Count Five test statistic)

x1 <- rnorm(20, 0, sd = 1)
x2 <- rnorm(20, 0, sd = 1.5)
y <- c(x1, x2)

group <- rep(1:2, each = length(x1))
boxplot(y ~ group, boxwex = .3, xlim = c(.5, 2.5), main = "")
points(group, y)
# boxplot(cbind(x1,x2), boxwex = .3)

# now identify the extreme points
range(x1)
range(x2)

i <- which(x1 < min(x2))
j <- which(x2 > max(x1))

x1[i]
x2[j]

out1 <- sum(x1 > max(x2)) + sum(x1 < min(x2))
out2 <- sum(x2 > max(x1)) + sum(x2 < min(x1))
max(c(out1, out2))


### Example 6.13 (Count Five test statistic, cont.)

maxout <- function(x, y) {
  X <- x - mean(x)
  Y <- y - mean(y)
  outx <- sum(X > max(Y)) + sum(X < min(Y))
  outy <- sum(Y > max(X)) + sum(Y < min(X))
  return(max(c(outx, outy)))
}

n1 <- n2 <- 20
mu1 <- mu2 <- 0
sigma1 <- sigma2 <- 1
m <- 1000

# generate samples under H0
set.seed(123)
stat <- replicate(m, expr={
  x <- rnorm(n1, mu1, sigma1)
  y <- rnorm(n2, mu2, sigma2)
  maxout(x, y)
})
print(cumsum(table(stat)) / m)
print(quantile(stat, c(.8, .9, .95)))


### Example 6.14 (Count Five test)

count5test <- function(x, y) {
  X <- x - mean(x)
  Y <- y - mean(y)
  outx <- sum(X > max(Y)) + sum(X < min(Y))
  outy <- sum(Y > max(X)) + sum(Y < min(X))
  # return 1 (reject) or 0 (do not reject H0)
  return(as.integer(max(c(outx, outy)) > 5))
}

n1 <- n2 <- 20
mu1 <- mu2 <- 0
sigma1 <-  sigma2 <- 1
m <- 10000
tests <- replicate(m, expr = {
  x <- rnorm(n1, mu1, sigma1)
  y <- rnorm(n2, mu2, sigma2)
  x <- x - mean(x)  #centered by sample mean
  y <- y - mean(y)
  count5test(x, y)
} )

alphahat <- mean(tests)
print(alphahat)


### Example 6.15 (Count Five test, cont.)

n1 <- 20
n2 <- 30
mu1 <- mu2 <- 0
sigma1 <- sigma2 <- 1
m <- 10000

alphahat <- mean(replicate(m, expr={
  x <- rnorm(n1, mu1, sigma1)
  y <- rnorm(n2, mu2, sigma2)
  x <- x - mean(x)  #centered by sample mean
  y <- y - mean(y)
  count5test(x, y)
}))

print(alphahat)


### Example 6.16 (Count Five, cont.)

# generate samples under H1 to estimate power
sigma1 <- 1
sigma2 <- 1.5

power <- mean(replicate(m, expr={
  x <- rnorm(20, 0, sigma1)
  y <- rnorm(20, 0, sigma2)
  count5test(x, y)
}))

print(power)

