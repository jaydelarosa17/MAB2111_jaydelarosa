# R function that computes the factorial of given an integer argument. The output should be a vector of length 1.

factorial <- function(x) {
  if (x == 0 || x == 1) {
    return (1)}
      else {return(x*factorial(x-1))}
  }

#sample:

factorial(5)
#[1] 120
factorial(4)
#[1] 24
factorial(3)
#[1] 6
factorial(1)
#[1] 1


# R function that computes the determinant of a given matrix. The output should be a vector of length 1.

# 2x2 matrix:  

mdeterm <- function(mdeterm){
  a <- mdeterm[1,1]
  b <- mdeterm[1,2]
  c <- mdeterm[2,1]
  d <- mdeterm[2,2]
  return (a*d - b*c)} 

Test = matrix(c(0,9,17,81),2,2)

mdeterm(Test)
#[1] -153


# R function that sorts a given vector in decreasing order. The output should be a vector of the same length. It should accept both numeric or character vectors.

msorts <- function(myvex){
  for (j in 2:length(myvex)) {
    init = myvex[j] 
    i = j - 1 
    while (i > 0 && myvex[i] < init) {
      myvex[(i + 1)] = myvex[i]
      i = i - 1 
    }
    myvex[(i + 1)] = init
  }
  return (myvex)
} 

# Samples:

num = c(1,2,3,4)
msorts(num)

# [1] 4 3 2 1

char = c('a','b','c','d','e','f')
msorts(char)  

# [1] "f" "e" "d" "c" "b" "a"


# function that computes the compound interest of an investment given the rate, time, and initial amount or principal.

Future_value <- function(principal, rate = 0.04, no_periods = 5)
    {
    return (principal * ((1 + rate)**no_periods))
}

Future_value(100)

# [1] 121.6653


# function isPrime(n) that accepts an integer and outlibraryputs a Boolean value (TRUE or FALSE) depending whether the integer is a prime number or not.

is.prime <- function(num) {
  if (num == 2) {
    TRUE
  } else if (any(num %% 2:(num-1) == 0)) {
    FALSE
  } else { 
    TRUE
  }
}

#sample:
is.prime(2)
#[1] TRUE
is.prime(3)
#[1] TRUE
is.prime(4)
#[1] FALSE



