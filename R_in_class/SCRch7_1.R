### Example 7.1
set.seed(8)
x=rpois(10,2)
x
table(x)
hist(x)
par(mfrow=c(1,2))
plot(ecdf(x))
plot(ecdf(rpois(1000,2)), main='Poi(2)', ylab="F(x)")
dev.off()


### Example 7.2 (Bootstrap estimate of standard error)

library(bootstrap)    #for the law data
cor(law$LSAT, law$GPA)
cor(law82$LSAT, law82$GPA)

#set up the bootstrap
B <- 200            #number of replicates
n <- nrow(law)      #sample size
R <- numeric(B)     #storage for replicates

#bootstrap estimate of standard error of R
for (b in 1:B) {
  #randomly select the indices
  i <- sample(1:n, size = n, replace = TRUE)
  LSAT <- law$LSAT[i]       #i is a vector of indices
  GPA <- law$GPA[i]
  R[b] <- cor(LSAT, GPA)
}
#output
se.R <- sd(R)
se.R

hist(R, prob = TRUE)
abline(v=cor(law$LSAT, law$GPA))

### Example 7.3 (Bootstrap estimate of standard error: boot function)

r <- function(x, i) {
  #want correlation of columns 1 and 2
  cor(x[i,1], x[i,2])
}

library(boot)       #for boot function
?boot    # check statistic
obj <- boot(data = law, statistic = r, R = 2000)
obj
y <- obj$t
sd(y)


### Example 7.4 (Bootstrap estimate of bias)

#sample estimate for n=15
theta.hat <- cor(law$LSAT, law$GPA)

#bootstrap estimate of bias
B <- 2000   #larger for estimating bias
n <- nrow(law)
theta.b <- numeric(B)

for (b in 1:B) {
  i <- sample(1:n, size = n, replace = TRUE)
  LSAT <- law$LSAT[i]
  GPA <- law$GPA[i]
  theta.b[b] <- cor(LSAT, GPA)
}
bias <- mean(theta.b - theta.hat)
bias
# compare
obj


### Example 7.5 (Bootstrap estimate of bias of a ratio estimate)

data(patch, package = "bootstrap")
patch

n <- nrow(patch)  #in bootstrap package
B <- 2000
theta.b <- numeric(B)
theta.hat <- mean(patch$y) / mean(patch$z)

#bootstrap
for (b in 1:B) {
  i <- sample(1:n, size = n, replace = TRUE)
  y <- patch$y[i]
  z <- patch$z[i]
  theta.b[b] <- mean(y) / mean(z)
}
bias <- mean(theta.b) - theta.hat
se <- sd(theta.b)
print(list(est=theta.hat, bias = bias,
           se = se))


#################
### Example 7.10 (Bootstrap confidence intervals for patch ratio statistic)

library(boot)       #for boot and boot.ci
data(patch, package = "bootstrap")

theta.boot <- function(dat, ind) {
  #function to compute the statistic
  y <- dat[ind, 1]
  z <- dat[ind, 2]
  mean(y) / mean(z)
}

y <- patch$y
z <- patch$z
dat <- cbind(y, z)
boot.obj <- boot(dat, statistic = theta.boot, R = 2000)

print(boot.obj)
print(boot.ci(boot.obj,
              type = c("basic", "norm", "perc")))


#calculations for bootstrap confidence intervals
alpha <- c(.025, .975)

#normal
print(boot.obj$t0 + qnorm(alpha) * sd(boot.obj$t))

#basic
print(2*boot.obj$t0 - 
        quantile(boot.obj$t, rev(alpha), type=1))

#percentile
print(quantile(boot.obj$t, alpha, type=6))


### Example 7.11 (Bootstrap confidence intervals for the correlation statistic)

library(boot)
data(law, package = "bootstrap")
boot.obj <- boot(law, R = 2000,
                 statistic = function(x, i){cor(x[i,1], x[i,2])})
print(boot.ci(boot.obj, type=c("basic","norm","perc")))


### Example 7.12 (Bootstrap t confidence interval)

boot.t.ci <- function(x, B = 500, R = 100, level = .95, statistic){
    #compute the bootstrap t CI
    x <- as.matrix(x);  n <- nrow(x)
    stat <- numeric(B); se <- numeric(B)
    
    boot.se <- function(x, R, f) {
      #local function to compute the bootstrap
      #estimate of standard error for statistic f(x)
      x <- as.matrix(x); m <- nrow(x)
      th <- replicate(R, expr = {
        i <- sample(1:m, size = m, replace = TRUE)
        f(x[i, ])
      })
      return(sd(th))
    }
    
    for (b in 1:B) {
      j <- sample(1:n, size = n, replace = TRUE)
      y <- x[j, ]
      stat[b] <- statistic(y)
      se[b] <- boot.se(y, R = R, f = statistic)
    }
    stat0 <- statistic(x)
    t.stats <- (stat - stat0) / se
    se0 <- sd(stat)
    alpha <- 1 - level
    Qt <- quantile(t.stats, c(alpha/2, 1-alpha/2), type = 1)
    names(Qt) <- rev(names(Qt))
    CI <- rev(stat0 - Qt * se0)
  }


### Example 7.13 (Bootstrap t confidence interval for patch ratio statistic)

#boot package and patch data were loaded in Example 7.10
library(boot)       #for boot and boot.ci
data(patch, package = "bootstrap")

dat <- cbind(patch$y, patch$z)
stat <- function(dat) {
  mean(dat[, 1]) / mean(dat[, 2]) 
  }
ci <- boot.t.ci(dat, statistic = stat, B=2000, R=200)
print(ci)


### Example 7.14 (BCa bootstrap confidence interval)
boot.BCa <- function(x, th0, th, stat, conf = .95) {
    # bootstrap with BCa bootstrap confidence interval
    # th0 is the observed statistic
    # th is the vector of bootstrap replicates
    # stat is the function to compute the statistic
    
    x <- as.matrix(x)
    n <- nrow(x) #observations in rows
    N <- 1:n
    alpha <- (1 + c(-conf, conf))/2
    zalpha <- qnorm(alpha)
    
    # the bias correction factor
    z0 <- qnorm(sum(th < th0) / length(th))
    
    # the acceleration factor (jackknife est.)
    th.jack <- numeric(n)
    for (i in 1:n) {
      J <- N[1:(n-1)]
      th.jack[i] <- stat(x[-i, ], J)
    }
    L <- mean(th.jack) - th.jack
    a <- sum(L^3)/(6 * sum(L^2)^1.5)
    
    # BCa conf. limits
    adj.alpha <- pnorm(z0 + (z0+zalpha)/(1-a*(z0+zalpha)))
    limits <- quantile(th, adj.alpha, type=6)
    return(list("est"=th0, "BCa"=limits))
  }


### Example 7.15 (BCa bootstrap confidence interval)

#boot package and patch data were loaded in Example 7.10
#library(boot)       #for boot and boot.ci
#data(patch, package = "bootstrap")

n <- nrow(patch)
B <- 2000
y <- patch$y
z <- patch$z
x <- cbind(y, z)
theta.b <- numeric(B)
theta.hat <- mean(y) / mean(z)

#bootstrap
for (b in 1:B) {
  i <- sample(1:n, size = n, replace = TRUE)
  y <- patch$y[i]
  z <- patch$z[i]
  theta.b[b] <- mean(y) / mean(z)
}
#compute the BCa interval
stat <- function(dat, index) {
  mean(dat[index, 1]) / mean(dat[index, 2])  }

boot.BCa(x, th0 = theta.hat, th = theta.b, stat = stat)



### Example 7.16 (BCa bootstrap confidence interval using boot.ci)

#using x from Example 7.15
boot.obj <- boot(x, statistic = stat, R=2000)
boot.ci(boot.obj, type=c("perc", "bca"))
