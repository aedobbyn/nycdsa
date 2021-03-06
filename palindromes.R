### Find the largest palindrome made from the product of two 3-digit numbers. ###

library(tidyverse)

# --------------- Simpler Solution ------------
# We assume that numbers must be whole and that our palindrome will be 6 digits long

find_palindromes_simple <- function(a = 999, b = 999) {
  for (i in 999:100) {
    product <- a*b
    
    # Take product to character and split into a vector of its digits
    product_str_split <- product %>% as.character() %>% strsplit("") %>% unlist()

    if (product_str_split[1] == product_str_split[6] & 
        product_str_split[2] == product_str_split[5] &
        product_str_split[3] == product_str_split[4]) {
      return(product)
      
    } else {   # Decrement either a or b by 1
      if (i %% 2 == 0) {
        a <- a - 1
      } else {
        b <- b - 1
      }
    }
  }
}

find_palindromes_simple()
# 698896


# ------------------- Safer soltuion ------------------
# We do not assume that our palindrome is 6 digits and we also don't assume 
# that numbers are whole or positive

# Helper function to be used in find_palindromes_safe()
check_equal <- function(obj) {
  len <- length(obj)
  if (len %% 2 == 0) {    
    lefts <- 1:(len/2)
    rights <- len:(max(lefts) + 1)    
  } else {                              # If we have an odd number of digits, skip the middle one
    lefts <- floor(1:(len/2))
    rights <- len:(max(lefts) + 2)
  }
  
  if (all(obj[lefts] == obj[rights])) {  # Check equality
    return(TRUE)
  } else {
    return(FALSE)
  }
}

# Test check_equal()
foo <- c("1", "2", "3,", "2", "1")      # check_equal(foo) yields TRUE
bar <- c("1", "2", "3", "3", "2", "1")  # check_equal(bar) yields TRUE
baz <- c("4", "5", "6,", "7", "8")      # check_equal(baz) yields FALSE


find_palindromes_safe <- function(start = 999, end = 100, by = 1) {
  a <- start
  b <- start
  if (start > end) {
    tries <- seq(end, start, by = by)
  } else {   # If start and end are negative
    tries <- seq(start, end, by = by)
  }
  
  for (i in tries) {
    product <- a*b
    
    product_str_split <- product %>% as.character() %>% 
      stringr::str_extract_all('[0-9]', simplify = TRUE) %>%  # Remove decimals and negative signs if they exist
      stringr::str_split("") %>% purrr::as_vector()
    
    if (check_equal(product_str_split) == TRUE) {
      return(product)
    } else {
      if (start > end & a > end & b > end) {
        if (i %% 2 == 0) {
          a <- a - by
        } else {
          b <- b - by
        }
      } else if (a < end & b < end) {  # Start and end are negative
        if (i %% 2 == 0) {
          a <- a + by
        } else {
          b <- b + by
        }
      } else {
        message("No palindromes in this range.")
      }
    }
  }
}

find_palindromes_safe()
# 698896

find_palindromes_safe(-999, -100, by = 1)
# 698896

find_palindromes_safe(3.14, 2.72, by = 0.001)
# No palindromes in this range.







