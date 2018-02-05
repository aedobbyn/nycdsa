

check_equal <- function(obj) {
  obj <- obj %>% as.character() %>% 
    stringr::str_extract_all('[0-9]', simplify = TRUE) %>%  # Remove decimals and negative signs if they exist
    stringr::str_split("") %>% purrr::as_vector()
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


check_equal <- function(obj) {

  len <- nchar(obj)
  if (len %% 2 == 0) {    
    lefts <- 1:(len/2)
    rights <- len:(max(lefts) + 1)    
  } else {                              # If we have an odd number of digits, skip the middle one
    lefts <- floor(1:(len/2))
    rights <- len:(max(lefts) + 2)
  }
  
  if (all(
    obj %>% substr(min(lefts), max(lefts)) == obj %>% substr(min(rights), max(rights))
    # obj[lefts] == obj[rights])
    )) {  # Check equality    
    return(TRUE)
  } else {
    return(FALSE)
  }
}


one <- 999:100
two <- 999:100

# mult <- one*two 
mult_df <- expand.grid(one, two) %>% 
  mutate(
    mult = Var1 * Var2
  ) %>% as_tibble() %>% 
  arrange(desc(Var1, Var2))



mult_df %>% 
  mutate(
    palindrome = map_lgl(mult, check_equal_2)
  ) %>% 
  filter(palindrome == TRUE) %>% 
  slice(1:1)


foo <- mult_df[1:10, ]

stringify <- function(e) {
  e <- e %>% as.character() %>% 
    stringr::str_extract_all('[0-9]', simplify = TRUE) %>%  # Remove decimals and negative signs if they exist
    stringr::str_split("") %>% purrr::as_vector()
  return(e)
}

bar <- foo %>% mutate(
  one_as_str = Var1 %>% map(stringify) %>% list()
  # two_as_str = Var2 %>% stringify() %>% nest()
)

