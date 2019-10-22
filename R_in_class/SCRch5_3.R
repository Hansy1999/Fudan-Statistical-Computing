
### Example 5.10 (Choice of the importance function)
m <- 10000
theta.hat <- se <- numeric(5)
g <- function(x) {
    exp(-x - log(1+x^2)) * (x > 0) * (x < 1)
}

x <- runif(m)     #using f0
fg <- g(x)
theta.hat[1] <- mean(fg)
se[1] <- sd(fg)

x <- rexp(m, 1)   #using f1
fg <- g(x) / exp(-x)
theta.hat[2] <- mean(fg)
se[2] <- sd(fg)

x <- rcauchy(m)   #using f2
i <- c(which(x > 1), which(x < 0))
x[i] <- 2  #to catch overflow errors in g(x)
fg <- g(x) / dcauchy(x)
theta.hat[3] <- mean(fg)
se[3] <- sd(fg)

u <- runif(m)     #f3, inverse transform method
x <- - log(1 - u * (1 - exp(-1)))
fg <- g(x) / (exp(-x) / (1 - exp(-1)))
theta.hat[4] <- mean(fg)
se[4] <- sd(fg)

u <- runif(m)    #f4, inverse transform method
x <- tan(pi * u / 4)
fg <- g(x) / (4 / ((1 + x^2) * pi))
theta.hat[5] <- mean(fg)
se[5] <- sd(fg)

round(rbind(theta.hat, se/sqrt(m)),digits=4)



### Plot importance functions in Figures 5.1(a) and 5.1.(b)
#par(ask = TRUE) #uncomment to pause between graphs

x <- seq(0, 1, .01)
w <- 2
f1 <- exp(-x)
f2 <- (1 / pi) / (1 + x^2)
f3 <- exp(-x) / (1 - exp(-1))
f4 <- 4 / ((1 + x^2) * pi)
g <- exp(-x) / (1 + x^2)

#for color change lty to col

#figure (a)
plot(x, g, type = "l", main = "envelop", ylab = "",
     ylim = c(0,2), lwd = w, col=1)
lines(x, g/g, col = 2, lwd = w)
lines(x, f1, col = 3, lwd = w)
lines(x, f2, col = 4, lwd = w)
lines(x, f3, col = 5, lwd = w)
lines(x, f4, col = 6, lwd = w)
legend("topright", legend = c("g", 0:4),
       col = 1:6, lwd = w, inset = 0.02)

#figure (b)
plot(x, g, type = "l", main = 'ratio of g over envelop', 
     ylab = "", ylim = c(0,3.2), lwd = w, col = 2)
lines(x, g/f1, col = 3, lwd = w)
lines(x, g/f2, col = 4, lwd = w)
lines(x, g/f3, col = 5, lwd = w)
lines(x, g/f4, col = 6, lwd = w)
legend("topright", legend = c(0:4),
       col = 2:6, lwd = w, inset = 0.02)


### Example 5.11 (Example 5.10, cont.)
set.seed(123)

M <- 20   #number of replicates
T2 <- numeric(4)
estimates <- matrix(0, 10, 2)

g <- function(x) {
    exp(-x - log(1+x^2)) * (x > 0) * (x < 1) }

for (i in 1:10) {
    estimates[i, 1] <- mean(g(runif(M)))
    T2[1] <- mean(g(runif(M/4, 0, .25)))
    T2[2] <- mean(g(runif(M/4, .25, .5)))
    T2[3] <- mean(g(runif(M/4, .5, .75)))
    T2[4] <- mean(g(runif(M/4, .75, 1)))
    estimates[i, 2] <- mean(T2)
}

estimates
apply(estimates, 2, mean)
my.var = apply(estimates, 2, var)
my.var
(my.var[1]-my.var[2])/my.var[1]

### Example 5.12 (Examples 5.10-5.11, cont.)

M <- 10000  #number of replicates
k <- 10     #number of strata
r <- M / k  #replicates per stratum
N <- 50     #number of times to repeat the estimation
T2 <- numeric(k)
estimates <- matrix(0, N, 2)

g <- function(x) {
    exp(-x - log(1+x^2)) * (x > 0) * (x < 1)
}

for (i in 1:N) {
    estimates[i, 1] <- mean(g(runif(M)))
    for (j in 1:k)
        T2[j] <- mean(g(runif(M/k, (j-1)/k, j/k)))
    estimates[i, 2] <- mean(T2)
}

apply(estimates, 2, mean)
apply(estimates, 2, var)

(var(estimates[,1])-var(estimates[,2]))/var(estimates[,1])
