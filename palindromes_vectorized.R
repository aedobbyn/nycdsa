

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


one <- 999:100
two <- 999:100

mult <- one*two 

mult %>% as_tibble() %>% 
  mutate(
    palindrome = map_lgl(value, check_equal_2)
  ) %>% 
  filter(palindrome == TRUE) %>% 
  slice(1:1)

