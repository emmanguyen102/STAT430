library(bootstrap)
library(boot)

## Ex1:
s <- c(7.0, 19.8, 12.8, 6.0, 15.2, 5.1, 15.0, 7.6)

# trimmed mean
trm <- mean(s, trim=0.25)

# calculate the leave-one-out means
l1o <- vector(length=8)
for (i in 1:8){
  l1o[i] <- mean(s[-i], trim=0.25)
}
theta.bar <- mean(l1o)

# estimated bias
bias <- 7*(trm-theta.bar)
bias

## Ex2:
a <- c(1,1,2,3,3)

mean(a)

l1o <- vector(length=5)
for (i in 1:5){
  l1o[i] <- mean(s[-i])
}
mean(l1o)


## Ex3:
set.seed(1000)
attach(faithful)
x <- cbind(eruptions, waiting)
n <- length(eruptions)
# 1000 Bootstrap samples
m <- 1000
b <- numeric(m)
for(i in 1:m) {
  # Bootstrap sample
  ind <- sample(1:n, n, replace=T)
  theta <- x[ind,]
  # Correlation of the sample
  b[i] <- cor(theta[,1], theta[,2])
}
# Percentile interval
PI <- quantile(b, probs = c(0.025, 0.975))
PI

# boot() can be used as well to calculate the intervals
theta <- function(m,x) {
  cor(m[x,1], m[x,2])
}
l <- boot(cbind(eruptions, waiting), theta, R=1000)
# Normal CI
boot.ci(l, type = "norm")

# Basic CI
boot.ci(l, type = "basic")

## Ex4
set.seed(1000)
# Generate the sample from the normal distribution
s <- rnorm(20)
# Bootstrap of mean
theta1 <- function(x,i) {
  mean(x[i])
}
l1 <- boot(s, theta1, R=1000)
# Bootstrap of median
theta2 <- function(x,i) {
  median(x[i])
}
l2 <- boot(s, theta2, R=1000)
# Variances
var(l1$t)

var(l2$t)

# bootstrap() can be used as well
theta3 <- function(x) {mean(x)}
theta4 <- function(x) {median(x)}
l3 <- bootstrap(s, 1000, theta3)
l4 <- bootstrap(s, 1000, theta4)
var(l3$thetastar)

var(l4$thetastar)


## Ex5:
set.seed(1000)
# Give the data in an appropriate form
x <- c(22, 23.4, 24.9, 28.5, 29.8, 31.6, 34.2, 36.4, 37.7, 39)
y <- c(96, 88, 105, 111, 107, 113, 132, 122, 135, 131)
data <- cbind(x,y)
# Jackknife
theta1 <- function(x,m){
  cor(m[x,1],m[x,2])
}
l1 <- jackknife(1:10, theta1, data)
# Pseudovalues and the Jackknife-estimator
bjack<-10*cor(x,y)-9*l1$jack.values
theta.est <- sum(bjack) / 10
# Confidence interval
t.value <- qt(0.975, 9)
dev <- t.value*sqrt(var(bjack) / 10)
lower <- theta.est - dev
upper <- theta.est + dev
CI <- c(lower, upper)
CI

# Bootstrap
theta2 <- function(m,x) {
  cor(m[x,1], m[x,2])
}
l2 <- boot(data, theta2, R=1000)
boot.ci(l2, type = "perc")
