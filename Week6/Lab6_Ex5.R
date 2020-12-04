# Ex5

set.seed(123)

mu <- 3
sigma <- 4
n <- 30
theta <- 2

sample <- rnorm(n, mu, sqrt(sigma))
sample_mean <- mean(sample)

testing <- c(0.25, 1, 4)
for (square_tau in testing){
  w <- square_tau/(square_tau+sigma/n)
  m <- w*sample_mean + (1-w)*theta
  square_w <- (n/sigma + 1/square_tau)^-1
  bayes1=rnorm(10000, m, sqrt(square_w))
}








