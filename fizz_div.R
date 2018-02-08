
### Find the sum of all numbers below 1000 that are divisible by 3 or 5 ###

# Here assuming numbers must be positive

library(tidyverse)

#### While loop approach ####

fizz_div_while <- function(min = 0, max = 1000, by = 1, gte = FALSE) {
  
  # Should max be inclusive or exclusive?
  if (gte == FALSE) {
    max <- max - by
  }
  x <- max
  sum <- 0 
  while (x > min) {
    if (x %% 3 == 0 | x %% 5 == 0) {
      sum <- sum + x
    }
    x <- x - by
  }
  return(sum)
}

fizz_div_while()
# 233168


##### Vectorized approach #####

# Helper for determining whether a single number is divisible by 3 or 5
keep_it <- function(e) {
  if (e %% 3 == 0 || e %% 5 == 0)  {
    out <- TRUE
  } else {
    out <- FALSE
  }
  return(out)
}

keep_it(9)
# TRUE
keep_it(11)
# FALSE


fizz_div_vec <- function(min = 0, max = 1000, by = 1, gte = FALSE) {
  
  # Should max be inclusive or exclusive?
  if (gte == FALSE) {
    max <- max - by
  }
  num_space <- seq(min, max, by = by)
  
  # Add a column to our tibble for whether we're adding this row to our sum
  num_space <- num_space %>% as_tibble() %>% 
    rowwise() %>% 
    mutate(
      to_keep = ifelse(keep_it(value), TRUE, FALSE)
    )
  
  out <- num_space %>% filter(
      to_keep == TRUE
    ) %>% ungroup() %>% 
    summarise(
      total = sum(value)
    ) %>% 
    pluck("total")
  
  return(out)
}


fizz_div_vec()
# 233168


# ---- Compare runtimes -----

system.time(fizz_div_while())
# user  system elapsed 
# 0.001   0.000   0.001 
system.time(fizz_div_vec())
# user  system elapsed 
# 0.046   0.001   0.047 
# ^^^ 0.03 seconds faster without the intermediate assignment to variables num_space

# Given all this, I'd go with fizz_div_while()

