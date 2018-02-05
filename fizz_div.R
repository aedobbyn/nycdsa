
### Find the sum of all numbers below 1000 that are divisible by 3 or 5 ###

# Assuming numbers must be positive

fizz_div <- function(decrement = 1) {
  sum <- 0 
  x <- 1000 - decrement
  while (x > 0) {
    if (x %% 3 == 0 | x %% 5 == 0) {
      sum <- sum + x
    }
    x <- x - decrement
  }
  return(sum)
}

fizz_div()
# 233168

