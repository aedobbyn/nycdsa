library(tidyverse)


# Helper to turn a number into its individual digits
stringify <- function(e) {
  e <- e %>% as.character() %>% 
    stringr::str_extract_all('[0-9]', simplify = TRUE) %>%  # Remove decimals and negative signs if they exist
    stringr::str_split("") %>% purrr::as_vector()
  return(e)
}

stringify(123)
# "1" "2" "3"


# Helper to check if 
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

12321 %>% stringify() %>% check_equal()
# TRUE


# Find palindromes by specifying a start and end numbers to multiply and percent of the multiples to be tested 
# This is done in order to take advantage of fast apply-based functions in the mutate. We can't tell these to `break` 
# once they hit upon an answer, so we'll check in small batches whether they have
find_palindromes <- function(start = 999, end = 100, batch_percent = 0.001) {
  a <- start:end
  b <- start:end
  
  # Expand one and two into all their possible combinations
  mult_df <- expand.grid(a, b) %>% 
    as_tibble() %>% 
    mutate(
      mult = Var1 * Var2
    ) %>% 
    arrange(desc(mult))
  
  # Define the beginning and end of our batches
  n_per_batch <- (nrow(mult_df)*batch_percent) %>% ceiling()   # ceiling() takes care of the case where our n_per_batch might be 0
  batch_start <- 1
  batch_end <- n_per_batch
  palindromes <- tibble()
    
  while (nrow(palindromes) == 0) {
    palindromes <- mult_df %>%
      slice(batch_start:batch_end) %>% 
      rowwise() %>%
      mutate(
        mult_as_str = mult %>% stringify() %>% list(),
        palindrome = mult_as_str %>% check_equal()
      ) %>% 
      filter(palindrome == TRUE) %>% 
      filter(mult == max(mult)) %>% 
      slice(1:1)
    
    # Re-up our batches if we couldn't find any palindromes
    batch_start <- batch_end
    batch_end <- batch_end + n_per_batch
    message(glue::glue("Testing up to row {batch_end}"))
    if (batch_end >= nrow(mult_df)) {
      message("No palindromes to be found.")
      break
    }
  }
  
  return(palindromes)
}

find_palindromes()

sol <- find_palindromes()
# Testing up to row 1620
# Testing up to row 2430
# Testing up to row 3240
# Testing up to row 4050
# Testing up to row 4860
# Testing up to row 5670
sol %>% pluck("mult")
# 906609

find_palindromes(1.23, 4.56)
# No palindromes in this range.


