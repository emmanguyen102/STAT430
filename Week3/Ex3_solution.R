##Ex1 

# a. Plot the anscombe data set
par(mfrow=c(2,2))
plot(anscombe$x1, anscombe$y1)
plot(anscombe$x2, anscombe$y2)
plot(anscombe$x3, anscombe$y3)
plot(anscombe$x4, anscombe$y4)

# b. Calculate OLS estimates for the regression models
print("b1")
b1 <- lsfit(anscombe$x1, anscombe$y1, intercept =TRUE)
b1$coefficients

print("b2")
b1 <- lsfit(anscombe$x2, anscombe$y2, intercept =TRUE)
b1$coefficients

print("b3")
b1 <- lsfit(anscombe$x3, anscombe$y3, intercept =TRUE)
b1$coefficients

print("b4")
b1 <- lsfit(anscombe$x4, anscombe$y4, intercept =TRUE)
b1$coefficients


## Ex2:

I <- diag(11)
J <- matrix(1/11, 11, 11)
x <- cbind(anscombe$y1, anscombe$y2, anscombe$y3, anscombe$y4)
sd <- c(sd(anscombe$y1), sd(anscombe$y2), sd(anscombe$y3), sd(anscombe$y4))

# Covariance matrix
S <- t(x)%*%(I-J)%*%x/10

# Correlation matrix
r <- solve(diag(sd)%*%S%*%solve(diag(sd)))
print("S")
print("r")


## Ex3:

alkuluku <- function(n){
  a <- 2:celling(sqrt(n))
  b <- rep(n, length(a))
  zeros <- rep(0, length(a))
  d <- b%%a
  if(all(d!=zeros)==TRUE){
    return(TRUE)
  }
  else {
    return(FALSE)
  }
}


## Ex4: 

ran <- function(a, C, m, seed, max) {
  vec <- vector(length=max)
  x <- seed %% m
  for(i in 1:max) {
    x <- (a*x + C) %% m
    vec[i] <- x
  }
  return(vec)
}

# a.
v1 <- ran(4,0,16,1000, 40)

# b.
v2 <- ran(5,1,16,1000, 40)



## Ex5:
# a.
# Returns n normally distributed (pseudo)-random numbers
rand.clt <- function(n) {
  vec <- vector(length=n)
  for(i in 1:n) {
    vec[i] = 12*mean(runif(12)) - 6
  }
  return(vec)
}

# Generate 10000 numbers and plot against the normal distribution
rvec <- rand.clt(10000)
plot(density(rvec))
curve(dnorm(x), add=TRUE, lty=2)

# b.
# Returns n normally distributed (pseudo)-random numbers
rand.bm <- function(n) {
  vec1 <- vector(length=n)
  vec2 <- vector(length=n)
  for(i in 1:n) {
    u <- runif(2)
    n1 <- (-2*log(u[1]))**0.5*cos(2*pi*u[2])
    n2 <- (-2*log(u[1]))**0.5*sin(2*pi*u[2])
    vec1[i] <- n1
    vec2[i] <- n2
  }
  return(cbind(vec1, vec2))
}
# Generate 10000 numbers and plot against the normal distribution
m <- rand.bm(10000)
par(mfrow=c(1,2))
plot(density(m[,1]))
curve(dnorm(x), add=TRUE, lty=2)
plot(density(m[,2]))
curve(dnorm(x), add=TRUE, lty=2)


## Ex6:
weibull2 <- function(lambda, C) {
  # Generate u from the runif()-function
  u <- runif(1)
  # Transform u to the Weibull-distribution using the transformation-function
  w <- (1/lambda)*(-log(1-u))**(1/C)
  return(w)
}
# Illustrating the function
c1 <- vector(length = 10000)
c2 <- vector(length = 10000)
c3 <- vector(length = 10000)
c4 <- vector(length = 10000)
for(i in 1:10000) {
  c1[i] <- weibull2(1, 0.5)
  c2[i] <- weibull2(1, 1)
  c3[i] <- weibull2(1, 1.5)
  c4[i] <- weibull2(1, 5)
}
# Curves with the different parameters.
plot(-1,-1, xlim = c(0,3), ylim = c(0,2))
lines(density(c1), col = 4) # Sininen
lines(density(c2), col = 2) # Punainen
lines(density(c3), col = 6) # Violetti
lines(density(c4), col = 3) # Vihreä

# When c=0.5 or c=1, the curve doesn't follow the theoretical curve well,
# but with c=1.5 and c=5 the approximation is quite good
par(mfrow= c(1,2))
plot(density(c3))
curve(dweibull(x, 1.5), add=TRUE, lty=2)
plot(density(c4))
curve(dweibull(x, 5), add=TRUE, lty=2)
