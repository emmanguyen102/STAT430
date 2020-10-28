## Ex1: A function that calculates terms for different 
## values of n
## a1=1
## a2 =1
## a_n+2_ = a_n + a_n+1_

fibonacci <- function(n){
  if (n < 0) {
    print("Please provide a positive integer")
  }
  else {
    count = 0
    a1=1
    a2=1
    print(a1)
    print(a2)
    while (count < n) {
      ath = a1+a2
      print(ath)
      
      ## update a1, a2
      a1=a2
      a2=ath
      count = count + 1
    }
  }
}


## Ex2: Create a function that calculates from a given
## DATA MATRIX the minimum, the median, the maximum and 
## so-called interquantile range

myList <- function(y) {
  y_min <- min(y)
  y_median <- median(y)
  y_max <- max(y)
  y_q1 <- quantile(y, 0.25)
  y_q2 <- quantile(y, 0.75)
  result <- list(min = y_min, 
              median = y_median, 
              max = y_max, 
              quantile_25 = y_q1, 
              quantile_75 = y_q2)
  result
}

## For testing
A <- matrix(nr=2, nc=4, data =1:6)
myList(A)


## Ex3: Use appropriate operations to form the following
## matrices

## a. 
## Zero matrix size 100*100: O(100x100)
O <- matrix(nr=100, nc=100, 0)

## Identity matrix size 100*100: I(100*100)
I <- diag(100)

## J(100*100)
J <- matrix(nr=100, nc=100, data=1/100)

## D(100*100)
D <- diag(1:100)

## b.Matrix A

i_20 = matrix(nr=20, data=1)
i_30 = matrix(nr=30, data=1)
i_50 = matrix(nr=50, data=1)

o_20 = matrix(nr=20, data=0)
o_30 = matrix(nr=30, data=0)
o_50 = matrix(nr=50, data=0)

A <- matrix(c(i_20, o_30, o_50, o_20, i_30, o_50, o_20, o_30, i_50), nc=3)

## Ex4:

## a. Show that AB is different from BA
A <- matrix(c(1,0,1,-2,4,2,3,2,1), nr=3)
B <- matrix(c(1,0,2,1,0,-1,2,4,3), nr=3)

## Proving that AB is different from BA
AB <- A%*%B 
BA <- B%*%A

## b. Show that AA' and A'A are symmetric, but not equal

## Transpose of A
t(A)

## Proving that AA' and A'A are not equal
A_mul_transposeA <- A%*%t(A)
transposeA_mul_A <- t(A)%*%A

## Proving that AA' and A'A are symmetric

t(A_mul_transposeA)
A_mul_transposeA - t(A_mul_transposeA)

t(transposeA_mul_A)
transposeA_mul_A - t(transposeA_mul_A)


## Ex5: 
i <- matrix(nr=5, data=1)
x <- matrix(nr=5, data=1:5)
X <- matrix(c(i,x), nc=2)

## a. Calculate X'X
X_transpose <- t(X)
X_transpose_mul_X <- t(X)%*%X

## b. Calculate (I-J)*X where J =(1/5*i*i')
j <- (1/5)*i%*%t(i)
I <- diag(5)

the_other <- (I-j)%*%X

## c. Calculate values of J1 and (I-J)1

J_mul_i <- j%*%i

the_last <- (I-j)%*%i

