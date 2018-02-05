library(tidyverse)


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

# Helper to turn a number into its individual digits
stringify <- function(e) {
  e <- e %>% as.character() %>% 
    stringr::str_extract_all('[0-9]', simplify = TRUE) %>%  # Remove decimals and negative signs if they exist
    stringr::str_split("") %>% purrr::as_vector()
  return(e)
}

stringify(123)
# "1" "2" "3"


# Set our 
one <- 999:100
two <- 999:100

# Expand one and two into all their possible combinations
mult_df <- expand.grid(one, two) %>% 
  mutate(
    mult = Var1 * Var2
  ) %>% as_tibble() %>% 
  arrange(desc(Var1, Var2))

mult_df_trues <- mult_df %>% 
  filter(mult == 698896) %>% 
  rowwise() %>%
  mutate(
    mult_as_str = mult %>% stringify() %>% list()
  ) 

mult_df_trues %>% 
  mutate(
    palindrome = check_equal(mult_as_str)
  ) %>% 
  filter(palindrome == TRUE) %>% 
  slice(1:1)


find_palindromes <- function(start = 999, end = 100) {
  a <- start:end
  b <- start:end
  
  # Expand one and two into all their possible combinations
  mult_df <- expand.grid(a, b) %>% 
    as_tibble() %>% 
    mutate(
      mult = Var1 * Var2
    ) %>% 
    arrange(desc(Var1, Var2))
  
  out <- mult_df %>%
    rowwise() %>%
    mutate(
      mult_as_str = mult %>% stringify() %>% list(),
      palindrome = mult_as_str %>% check_equal()
    ) %>% 
    filter(palindrome == TRUE) %>% 
    slice(1:1)
  
  return(out)
}

sol <- find_palindromes()



