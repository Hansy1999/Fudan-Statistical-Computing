#####################
### The Jackknife ###
#####################
### Example 7.6 (Jackknife estimate of bias)

data(patch, package = "bootstrap")
n <- nrow(patch)
y <- patch$y
z <- patch$z
theta.hat <- mean(y) / mean(z)
print (theta.hat)

#compute the jackknife replicates, leave-one-out estimates
theta.jack <- numeric(n)
for (i in 1:n)
  theta.jack[i] <- mean(y[-i]) / mean(z[-i])
bias <- (n - 1) * (mean(theta.jack) - theta.hat)

print(bias)  #jackknife estimate of bias


### Example 7.7 (Jackknife estimate of standard error)

se <- sqrt((n-1) *
             mean((theta.jack - mean(theta.jack))^2))
print(se)


### Example 7.8 (Failure of jackknife)
#for the specific example given
set.seed(1112)
#change the seed to see other examples
n <- 10
x <- sample(1:100, size = n)

#jackknife estimate of se
M.jack <- numeric(n)
for (i in 1:n) {        #leave one out
  y <- x[-i]
  M.jack[i] <- median(y)
}

Mbar <- mean(M.jack)
print(sqrt((n-1)/n * sum((M.jack - Mbar)^2)))

#bootstrap estimate of se
M.boot <- replicate(1000, expr = {
  y <- sample(x, size = n, replace = TRUE)
  median(y) })
print(sd(M.boot)) 

M.jack
head(M.boot)


### Example 7.17 (Model selection)

#to prompt for next graph, uncomment line below
#par(ask = TRUE)   

library(DAAG); attach(ironslag)
a <- seq(10, 40, .1)     #sequence for plotting fits

par(mfrow = c(2, 2))    #layout for graphs

L1 <- lm(magnetic ~ chemical)
plot(chemical, magnetic, main="Linear", pch=16)
yhat1 <- L1$coef[1] + L1$coef[2] * a
lines(a, yhat1, lwd=2)

L2 <- lm(magnetic ~ chemical + I(chemical^2))
plot(chemical, magnetic, main="Quadratic", pch=16)
yhat2 <- L2$coef[1] + L2$coef[2] * a + L2$coef[3] * a^2
lines(a, yhat2, lwd=2)

L3 <- lm(log(magnetic) ~ chemical)
plot(chemical, magnetic, main="Exponential", pch=16)
logyhat3 <- L3$coef[1] + L3$coef[2] * a
yhat3 <- exp(logyhat3)
lines(a, yhat3, lwd=2)

L4 <- lm(log(magnetic) ~ log(chemical))
plot(log(chemical), log(magnetic), main="Log-Log", pch=16)
logyhat4 <- L4$coef[1] + L4$coef[2] * log(a)
lines(log(a), logyhat4, lwd=2)

dev.off()
### Example 7.18 (Model selection: Cross validation)

# Example 7.17, cont.
n <- length(magnetic)   #in DAAG ironslag
e1 <- e2 <- e3 <- e4 <- numeric(n)

# for n-fold cross validation
# fit models on leave-one-out samples
for (k in 1:n) {
  y <- magnetic[-k]
  x <- chemical[-k]
  
  J1 <- lm(y ~ x)
  yhat1 <- J1$coef[1] + J1$coef[2] * chemical[k]
  e1[k] <- magnetic[k] - yhat1
  
  J2 <- lm(y ~ x + I(x^2))
  yhat2 <- J2$coef[1] + J2$coef[2] * chemical[k] +
    J2$coef[3] * chemical[k]^2
  e2[k] <- magnetic[k] - yhat2
  
  J3 <- lm(log(y) ~ x)
  logyhat3 <- J3$coef[1] + J3$coef[2] * chemical[k]
  yhat3 <- exp(logyhat3)
  e3[k] <- magnetic[k] - yhat3
  
  J4 <- lm(log(y) ~ log(x))
  logyhat4 <- J4$coef[1] + J4$coef[2] * log(chemical[k])
  yhat4 <- exp(logyhat4)
  e4[k] <- magnetic[k] - yhat4
}


c(mean(e1^2), mean(e2^2), mean(e3^2), mean(e4^2))

#selected model, fitted in Example 7.17
L2

par(mfrow = c(2, 1))    #layout for graphs
plot(L2$fit, L2$res)    #residuals vs fitted values
abline(0, 0)            #reference line
qqnorm(L2$res)          #normal probability plot
qqline(L2$res)          #reference line
dev.off()

