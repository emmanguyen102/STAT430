## Ex1:

## First approach: Use built-in function
require(MASS)

corMat <- matrix(cbind(1,0.8,0.3,
                   0.8,1,0.5,
                   0.3,0.5,1), nrow=3)

set.seed(1) 
out <- mvrnorm(n=1000, mu = c(0,0,0), Sigma = corMat, empirical = TRUE)
out <- as.matrix(out); out <- as.data.frame(out)
par(mfrow=c(1,3))
plot(out[,1], out[,2], xlab="x1",ylab="x2")
plot(out[,1], out[,3], xlab="x1",ylab="x3")
plot(out[,2], out[,3], xlab="x2",ylab="x3")

## Second approach: Define own function 
set.seed(1)

# Multivariate normal distribution, where mu = [0 0 0] and sigma is given
mu <- c(0,0,0)
sigma <- matrix(c(1,0.8,0.3,0.8,1,0.5,0.3,0.5,1) ,nrow=3, ncol=3)

# chol() gives the cholesky decomposition
C <- t(chol(sigma))

# generate n random variables from the distribution
genMN <- function(mu, C, n){
        # start with random standard normal variables
        randn <- matrix(rnorm(3*n), ncol=3)
        for (i in 1:n){
                randn[i,] <- C%*%randn[1,] + mu
        }
        return(randn)
 }
 
 a <- genMN(mu, C, 1000)
 par(mfrow=c(2,2))
plot(a[,1], a[,2], main="1&2; 0,8")
plot(a[,1], a[,3], main="1&3; 0,3")
plot(a[,2], a[,3], main="2&3; 0,5")


## Ex2:
set.seed(1000)

# Generate n random variables from f, where mu has the means of fi, w has the weights
# for f and var has the variances of fi
rwnorm <- function(mu, w,var, n){
        wvec <- vectir(length=n)
        for (i in 1:length(mu)){
                wvec <- wvec + w[i]*rnorm(n, mean=mu[i], sd=sqrt(var[i]))
        }
        return(wvec)
}

# generate 10000 random variables from f
rvec <- rwnorm((mu=c(110, 187, 229), w=c(0.28, 0.14, 0.58), var=c(354, 320, 845), n=10000)
plot(density(rvec), xlim =c(50,350))

# adding f1,f2,f3 to the plot as colored lines
curve(dnorm(x, mean = 110, sd = sqrt(354)), add = TRUE, col = "red",lty = 3)
curve(dnorm(x, mean = 187, sd = sqrt(320)), add = TRUE, col = "blue", lty = 3)
curve(dnorm(x, mean = 229, sd = sqrt(845)), add = TRUE, col = "green", lty = 3)


## Ex3: 
set.seed(1000)
# Sample and its mean
s <- c(0.13, -0.01, -0.01, 0.42, -0.02, 0.01, 0.09, 0.03, 0.04, 0.06, 0.12, 0.13)
sm <- mean(s)
# Funktio palauttaa n-rivisen matriisin, jossa rivit ovat satunnaisia permutaatioita otoksesta
# Returns an n-row matrix, where the rows are random permutations of the sample
randp <- function(s, n) {
rmatrix <- matrix(nrow = n, ncol = length(s))
for(i in 1:n) {
# Choose the sign with sample()
rmatrix[i,] <- s*sample(c(-1, 1), length(s), replace=T)
}
return(rmatrix)
}
n = 1000
# Test with 1000 permutations
rmatrix <- randp(s, n)
mu <- rowMeans(rmatrix)
hist(mu)
abline(v = sm)
abline(v = -sm)
p <- (sum(mu>sm)+sum(mu<(-sm)))/n
p



## Ex4:
set.seed(1000)
library(MASS)
library(bootstrap)
attach(wtloss)
# The fit-function for crossval(), n is the degree of the model
theta.fitn <- function(x,y,n){
a <- cbind(x)
if(n>1) {
for(i in 2:n) {
a <- cbind(a, I(x^i))
}
}
lsfit(a, y)
}
# The predict-function for crossval()
theta.predictn<-function(fit,x){
a <- cbind(1, x)
n <- length(fit$coef) - 1
if(n>1) {
for(i in 2:n) {
a <- cbind(a, I(x^i))
}
}
a%*%fit$coef
}
# Go through models up to the 10th degree
n=10
qs <- vector(length=n)
for(i in 1:n) {
results<-crossval(Days, Weight, theta.fitn, theta.predictn, n=i)
# Mean of the loss-function
Q <- sum((Weight-results$cv.fit)^2)/52
qs[i] <- Q
}
# Choose the degree with the smallest mean of the loss-function
# Degree
which.min(qs)

# Smallest Q
qs[which.min(qs)]



## Ex5:
set.seed(1000)
attach(sleep)
# Difference between the sample means of the groups
d <- mean(extra[1:10]) - mean(extra[11:20])
# 10000 random permutaion test
n=10000
m <- matrix(nr=20, nc=n)
for (i in 1:n){m[,i]<-sample(extra, 20)}
# Split the permutaions into the two groups and calculate their difference
gr1<-as.data.frame(m[1:10,])
gr2<-as.data.frame(m[11:20,])
v1<-sapply(gr1, mean)
v2<-sapply(gr2, mean)
# Distribution of the differences of the groups of the permutations,
# the vertical line is the difference between the sample means of the groups.
plot(density(v1-v2))
abline(v = d)
# Calculate the significance from the random permutations
p <- (sum((v1-v2)<d))/n
p
      
