## Ex 1: Calculate the value of function: y = 1+2x+3x^2

x = seq(1,100, by=1)
y = 1 + 2*x + 3*x^2
plot(x, y, type=”l”)


## Ex2: Calculation (Without loop):

## a. 1- 1/3 + 1/5 -1/7 + 1/9 - …
library(pracma)
suminf(k = 0, (-1)^k/(2*k+1))

## b.	1 + 1/3^2 + 1/5^2 + ...
suminf(k = 0, 1/((2*k+1)^2))

## c.	1 + 1/2^2 + 1/3^2 + 1/4^2 + ...
suminf(k= 0, 1/((k+1)^4))


## Ex3:	Create a data matrix (data.frame) from the given data:

## create teh data frame
st <- c(46, 54, 48, 50, 44, 42, 52)
d <- c(148, 182, 173, 166, 109, 141, 166)
n <- c("Ilkka (M)", "Kerttu (F)", "Eetu (M)", "Juuso (M)", "Ritva (F)", "Pentti (M)", "Maria (F)")

ques3_data <- data.frame("stretch" = st, "distance" = d, "name" = n, "sex" = factor(c("M", "F", "M", "M", "F", "M", "F")))
ques3_data

## summary of the data frame
summary(ques3_data)

## calculate the correlation between stretch and distance column
with(ques3_data, cor(stretch, distance))


## Ex4:	Create a list with given elements inside:

l <- list(dimension = dim(ques3_data), variable_names = names(ques3_data), 
          gender = summary(ques3_data$sex), stretch_mean = mean(ques3_data$stretch), 
          distance_mean = mean(ques3_data$distance))
l


## Ex5:	Calculate without looping:

## a.
a <- function(i) (-1)^k / factorial(i)
sumalt(a, 52)

## b.	
<- function(i) i^2 + 1
sumalt2(b, 100)

## c.	
c <- function(i) ((-1)^i)*(3^(2*i+1)) / factorial(2*i+1)
sumalt(c, 20)
