## Ex1: A function that calculates terms for different 
## values of n

fibonacci <- function(n){
  count = 0
  a1=1
  a2=1
  print(a1)
  print(a2)
  while (count < n) {
    ath = a1+a2
    print(ath)
    ## update a
    a1=a2
    a2=ath
    count = count + 1
  }
}

## Ex2: Create a function that calculates from a given
## data matrix the minimum, the median, the maximum and 
## so-called interquantile range

myList <- function(y) {
  y_min <- min(y)
  y_median <- median(y)
  y_max <- max(y)
  y_q1 <- quantile(y, 0.25)
  y_q2 <- quantile(y, 0.75)
  print (list(min = y_min, 
              median = y_median, 
              max = y_max, 
              quantile_25 = y_q1, 
              quantile_75 = y_q2))
}

## Ex3: 

