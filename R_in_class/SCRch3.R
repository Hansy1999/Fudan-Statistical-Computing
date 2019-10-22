### Example 3.1

#toss some coins
sample(0:1, size = 10, replace = TRUE)

#choose some lottery numbers
sample(1:100, size = 6, replace = FALSE)

#permuation of letters a-z
sample(letters)

#sample from a multinomial distribution
x <- sample(1:3, size = 10000, replace = TRUE, prob = c(.2, .3, .5))
table(x)


### Example 3.2 (Inverse transform method, continuous case)

n <- 1000
u <- runif(n)
x <- u^(1/3)
hist(x, prob = TRUE, main = expression(f(x)==3*x^2)) #density histogram of sample
#hist(x, prob = TRUE, main = bquote(f(x)==3*x^2)) #density histogram of sample

y <- seq(0, 1, .01)
lines(y, 3*y^2)    #density curve f(x)


### Example 3.4 (Two point distribution)

n <- 100000
p <- 0.4
u <- runif(n)
x <- as.integer(u > 0.6)   #(u > 0.6) is a logical vector

mean(x) # p=0.4
var(x)  # p*(1-p)=0.24


y <- rbinom(n, 1, p)
mean(y)
var(y)

### Example 3.5 (Geometric distribution)

n <- 1000
p <- 0.25
u <- runif(n)
k <- ceiling(log(1-u) / log(1-p)) - 1

# more efficient
k <- floor(log(u) / log(1-p))

k.R = rgeom(n, prob = p)
qqplot(k, k.R)
abline(0,1)


### Example 3.7 (Acceptance-rejection method)

n <- 1000
k <- 0      #counter for accepted
j <- 0      #iterations
y <- numeric(n)

while (k < n) {
u <- runif(1)
j <- j + 1
x <- runif(1)  #random variate from g
if (x * (1-x) > u) {
    #we accept x
    k <- k + 1
    y[k] <- x
}
}

j

# change c to be 1.5
n <- 1000
k <- 0      #counter for accepted
j <- 0      #iterations
y <- numeric(n)

while (k < n) {
u <- runif(1)
j <- j + 1
x <- runif(1)  #random variate from g
if (4*x * (1-x) > u) {
#we accept x
k <- k + 1
y[k] <- x
}
}

j

#compare empirical and theoretical percentiles
p <- seq(.1, .9, .1)
Qhat <- quantile(y, p)   #quantiles of sample
Q <- qbeta(p, 2, 2)      #theoretical quantiles
# se <- sqrt(p * (1-p) / (n * dbeta(Q, 2, 2)^2)) #see Ch. 2
round(rbind(Qhat, Q), 3)


### Verification of the Box-Muller 
n=1000
z1=rnorm(n); z2=rnorm(n)
u=runif(n)

qqplot(z1^2+z2^2, -2*log(u), cex=0.25, xlab="R2", ylab="-2logU")
abline(0, 1)


### Example 3.8 (Beta distribution)

n <- 1000
a <- 3
b <- 2
u <- rgamma(n, shape=a, rate=1)
v <- rgamma(n, shape=b, rate=1)
x <- u / (u + v)

q <- qbeta(ppoints(n), a, b)
qqplot(q, x, cex=0.25, xlab="Beta(3, 2)", ylab="Sample")
abline(0, 1)




### Example 3.10 (Chisquare)

n <- 10000
nu <- 2
X <- matrix(rnorm(n*nu), n, nu)^2 #matrix of sq. normals
#sum the squared normals across each row: 
#method 1
y <- rowSums(X)
#method 2
y <- apply(X, MARGIN=1, FUN=sum)  #a vector length n
mean(y)
var(y)

### Example (Convolutions and mixtures of normal dist)

n <- 10000
x1 <- rnorm(n, 0, 1)
x2 <- rnorm(n, 10, 1)
s <- x1 + x2              #the convolution
u <- runif(n)
k <- as.integer(u > 0.5)  #vector of 0's and 1's
x <- k * x1 + (1-k) * x2  #the mixture

par(mfcol=c(1,2))         #two graphs per page
hist(s, prob=TRUE, breaks = 30)
hist(x, prob=TRUE, breaks = 30)
par(mfcol=c(1,1))         #restore display



### Example 3.16 (Spectral decomposition method)

# mean and covariance parameters
mu <- c(0, 0)
Sigma <- matrix(c(1, .9, .9, 1), nrow = 2, ncol = 2)

rmvn.eigen <-
function(n, mu, Sigma) {
# generate n random vectors from MVN(mu, Sigma)
# dimension is inferred from mu and Sigma
d <- length(mu)
ev <- eigen(Sigma, symmetric = TRUE)
lambda <- ev$values
V <- ev$vectors
R <- V %*% diag(sqrt(lambda)) %*% t(V)
Z <- matrix(rnorm(n*d), nrow = n, ncol = d)
X <- Z %*% R + matrix(mu, n, d, byrow = TRUE)
X
}

# generate the sample
X <- rmvn.eigen(1000, mu, Sigma)

plot(X, xlab = "x", ylab = "y", pch = 20)
print(colMeans(X))
print(cor(X))


### Example 3.17 (SVD method)

rmvn.svd <-
function(n, mu, Sigma) {
# generate n random vectors from MVN(mu, Sigma)
# dimension is inferred from mu and Sigma
d <- length(mu)
S <- svd(Sigma)
R <- S$u %*% diag(sqrt(S$d)) %*% t(S$v) #sq. root Sigma
Z <- matrix(rnorm(n*d), nrow=n, ncol=d)
X <- Z %*% R + matrix(mu, n, d, byrow=TRUE)
X
}


### Example 3.18 (Choleski factorization method)

rmvn.Choleski <-
function(n, mu, Sigma) {
# generate n random vectors from MVN(mu, Sigma)
# dimension is inferred from mu and Sigma
d <- length(mu)
Q <- chol(Sigma) # Choleski factorization of Sigma
Z <- matrix(rnorm(n*d), nrow=n, ncol=d)
X <- Z %*% Q + matrix(mu, n, d, byrow=TRUE)
X
}
