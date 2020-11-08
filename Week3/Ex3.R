### Ex1: Data set anscombe (ready in R workspace) is used
### for this exercise. This data includes the variables x1, x2, x3
### and x4 as well as y1, y2, y3 and y4
summary(anscombe)

ff <- y ~ x
mods <- setNames(as.list(1:4), paste0("lm", 1:4))
for(i in 1:4) {
  ff[2:3] <- lapply(paste0(c("y","x"), i), as.name)
  ## or   ff[[2]] <- as.name(paste0("y", i))
  ##      ff[[3]] <- as.name(paste0("x", i))
  mods[[i]] <- lmi <- lm(ff, data = anscombe)
  print(anova(lmi))
}

## See how close they are (numerically!)
sapply(mods, coef)
lapply(mods, function(fm) coef(summary(fm)))

## Plots
op <- par(mfrow = c(2, 2), mar = 0.1+c(4,4,1,1), oma =  c(0, 0, 2, 0))
for(i in 1:4) {
  ff[2:3] <- lapply(paste0(c("y","x"), i), as.name)
  plot(ff, data = anscombe, col = "red", pch = 21, bg = "orange", cex = 1.2,
       xlim = c(3, 19), ylim = c(3, 13))
  abline(mods[[i]], col = "blue")
}
mtext("Anscombe's 4 Regression data sets", outer = TRUE, cex = 1.5)
par(op)



### Ex2: (continued) Calculate using matrix operations both covariance
### and correlation matrices from the variables y1,...,y4

y1 <- c(anscombe$y1)
y2 <- c(anscombe$y2)
y3 <- c(anscombe$y3)
y4 <- c(anscombe$y4)

a <- cbind(y1,y2,y3,y4)

k <- ncol(a) #number of variables: 4
n <- nrow(a) #number of subjects: 11

## 1. Covariance matrice
#create means for each column
a_mean <- matrix(nrow=n, data=1) %*% cbind(mean(y1),mean(y2),mean(y3),mean(y4)) 

#creates a difference matrix
b <- a - a_mean

#creates the covariance matrix
C <- ((n-1)^-1)*t(b) %*% b
##Testing: cov(a)


## 2.Correlation matrice:

##Testing: cor(a)
D <- diag(diag(C)^(-1/2))
  
R <- D %*% C %*% D


### Ex3: Make a function that tests whether a given number is a 
### prime number (you can use the R function %% [modulus]).
### Do not use looping!

check_prime_number <- function(n){
  if (n == 2 || n == 3){
    return("Prime number")
  } else if (n <= 1){
    return("Not a prime number")
  } else {
    if (((n+1)%%6 == 0) || (n-1)%%6 ==0){
      return("Prime number")
    }
    return("Not a prime number")
  }
}


### Ex4: Make a function that generates (pseudo) random number
###  using the linear congruence method. Explore the length of
###  the sequence when:

pseudo_generate<- function(a,c,m,run.length){
  mm <- vector(length =run.length)
  
  d <- as.numeric(Sys.time())*1000
  for (i in 1:run.length){
    d <- (a*d+c)%%m
    mm[i]<- d/m
  }
  return(mm)
}
## a) a = 4, c = 0 and m = 16 

pseudo_generate(a=4, c=0, m=16, run.length = 20)

## b) a = 5, c = 1 and m = 16
pseudo_generate(a=5, c=1, m=16, run.length = 1000)


### Ex5: Implement the generation of the values of the
### random variables from the standardized normal 
### distribution by:

## a) the method utilizing the central limit theorem (CLT)
CLT_normal <- function(n, m){
  # n: number of standardized normal random numbers to generate
  # m: the number of uniforms to generate for using CTL
  z <- rep(0,n)
  for (i in 1:n){
    u <- runif(m, 0,1)
    s <- sum(u)
    z[i] <- (s-m/2)/(m/12)
  }
  return(z)
}

par(mfrow=c(1,2)) 
m <- 1
x <- CLT_normal(1000, m)
qqnorm(x, main=paste("QQ normal m=", m))
qqline(x, col="red") 

m <- 30
x <- CLT_normal(1000, m)
qqnorm(x, main=paste("QQ normal m=", m))
qqline(x, col="red")

## b) the Box-Muller method. Illustrate the results graphically.
library(nortest)

size=10000

u = runif(size)
v = runif(size)

x= rep(0, size)
y=rep(0, size)

for(i in 1:size){
  x[i] = sqrt(-2*log(u[i]))*cos(2*pi*v[i])
  y[i] = sqrt(-2*log(u[i]))*sin(2*pi*v[i]) 
}

# plot the estimation of the density
plot(density((c(x,y))))

### Ex6:  Present and implement a procedure to obtain values of
### Weibull-distributed random variables 
### (do not use a ready R-function).  
### Illustrate the results with the values:
## (λ = 1, c = 0.5),
## (λ = 1, c = 1),
## (λ = 1, c = 1.5),
## (λ = 1, c = 5).

## (λ = 1, c = 0.5),
muy <- 1
c <- 0.5
u <- runif(100000,0,1)
u.sim(((-log(1-u))^(1/c))/muy) #using probability integral transformation

## (λ = 1, c = 1),
muy <- 1
c <- 1
u <- runif(100000,0,1)
u.sim<-(((-log(1-u))^(1/c))/muy)
log(5)
## (λ = 1, c = 1.5),
muy <- 1
c <- 1.5
u <- runif(100000,0,1)
u.sim<-(((-log(1-u))^(1/c))/muy)

## (λ = 1, c = 5).
muy <- 1
c <- 5
u <- runif(100000,0,1)
u.sim<-(((-log(1-u))^(1/c))/muy)

