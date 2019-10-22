
### Example 5.6 (Antithetic variables)

MC.Phi <- function(x, R = 10000, antithetic = TRUE) {
    u <- runif(R/2)
    if (!antithetic) {v <- runif(R/2)} else{
        v <- 1 - u
    }
    u <- c(u, v)
    cdf <- numeric(length(x))
    for (i in 1:length(x)) {
        g <- x[i] * exp(-(u * x[i])^2 / 2)
        cdf[i] <- mean(g) / sqrt(2 * pi) + 0.5
    }
    cdf
}

x <- seq(.1, 2.5, length=5)
Phi <- pnorm(x)
set.seed(123)
MC1 <- MC.Phi(x, anti = FALSE)
set.seed(123)
MC2 <- MC.Phi(x)
print(round(rbind(x, MC1, MC2, Phi), 5))


m <- 1000
MC1 <- MC2 <- numeric(m)
x <- 1.95
for (i in 1:m) {
    MC1[i] <- MC.Phi(x, R = 1000, anti = FALSE)
    MC2[i] <- MC.Phi(x, R = 1000)
}

print(sd(MC1))
print(sd(MC2))
print((var(MC1) - var(MC2))/var(MC1))


### Example 5.7 (Control variate)

m <- 10000
a <- - 12 + 6 * (exp(1) - 1)
U <- runif(m)
T1 <- exp(U)                  #simple MC
T2 <- exp(U) + a * (U - 1/2)  #controlled

c(mean(T1), sd(T1))   
c(mean(T2), sd(T2))  

(var(T1) - var(T2)) / var(T1)


### Example 5.8 (MC integration using control variates)

f <- function(u)
    exp(-.5)/(1+u^2)

g <- function(u)
    exp(-u)/(1+u^2)

set.seed(510) #needed later
u <- runif(10000)
B <- f(u)
A <- g(u)

cor(A, B)
a <- -cov(A,B) / var(B)    #est of c*
a

m <- 10000
u <- runif(m)
T1 <- g(u)
T2 <- T1 + a * (f(u) - exp(-.5)*pi/4)

c(mean(T1), mean(T2))
c(var(T1), var(T2))
(var(T1) - var(T2)) / var(T1)


### Example 5.9 (Control variate and regression)

set.seed(510)
u <- runif(10000)
f <- exp(-.5)/(1+u^2)
g <- exp(-u)/(1+u^2)
L <- lm(g ~ f)
summary(L)
c.star <-  - L$coeff[2]   # beta_1
mu <- exp(-.5)*pi/4

c.star

theta.hat <- sum(L$coeff * c(1, mu))  # pred. value at mu
predict(L, newdata = data.frame(f=mu)) # alternative

theta.hat
summary(L)$sigma^2
summary(L)$r.squared

