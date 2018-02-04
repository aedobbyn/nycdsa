
# Find the sum of all numbers below 1000 that are divisible by 3 or 5

## Assuming numbers must be positive and whole (so decrement step is 1)

fizz_div <- function(decrement = 1) {
  sum <- 0 
  x <- 1000 - decrement
  while (x > 0) {
    if (x %% 3 | x %% 5) {
      sum <- sum + x
    }
    x <- x - decrement
  }
  return(sum)
}

fizz_div()

