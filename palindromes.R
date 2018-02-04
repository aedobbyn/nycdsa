# Find the largest palindrome made from the product of two 3-digit numbers.

# Assume a*b is 6 digits

library(tidyverse)

find_palindromes <- function(a = 999, b = 999) {
  solution <- NULL
  while (is.null(solution)) {
    product <- a*b
    product_str <- as.character(product)
    
    product_str_split <- product_str %>% stringr::str_split("", simplify = TRUE)
    tot <- length(product_str_split)
    for (i in seq_along(product_str_split)) {
      j <- tot + 1 - i
      if (product_str_split[i] == product_str_split[j]) {
        if (i %% 2 == 0) {
          a <- a - 1
        } else {
          b <- b - 1
        }
      } else {
        solution <- product
        return(solution)
      }
    }
  }
}

find_palindromes()









### Assuming numbers must be whole

find_palindromes <- function(a = 999, b = 999) {
  for (i in 999:100) {
    product <- a*b
    
    # Take product to character and split into a vector of its digits
    product_str_split <- product %>% as.character() %>% strsplit("") %>% unlist()

    if (product_str_split[1] == product_str_split[6] & 
        product_str_split[2] == product_str_split[5] &
        product_str_split[3] == product_str_split[4]) {
      return(product)
      
    } else {
      if (i %% 2 == 0) {
        a <- a - 1
      } else {
        b <- b - 1
      }
    }
  }
}

find_palindromes()






find_palindromes_2 <- function(start = 999, end = 100) {
  a <- start
  b <- start
  for (i in start:end) {
    product <- a*b
    
    # Take product to character and split into a vector of its digits
    product_str_split <- product %>% as.character() %>% strsplit("") %>% unlist()
    
    len_product_str_split <- length(product_str_split)
    # if (len_product_str_split %% 2 == 0) {
      left <- 1
      right <- len_product_str_split
      while (right - left >= 1) {
        if (product_str_split[left] == product_str_split[right]) {
          left <- left + 1
          right <- right - 1
          return(product)  
# 
        }
#       } else {
#       
#       }
      left <- left + 1
      right <- right - 1
    }
    
      
    # } else {
      if (i %% 2 == 0) {
        a <- a - 1
      } else {
        b <- b - 1
      }
    }
  }


find_palindromes_2()

check_equal <- function(obj) {
  left <- 1
  right <- length(obj)
  while (right - left >= 1) {
    if (obj[left] == obj[right]) {
      left <- left + 1
      right <- right - 1
      return("yes")  
    } 
  }
}

check_equal <- function(obj) {
  i <- 1
  j <- length(obj)
  for (x in 1:length(obj)) {
    if (obj[i] == obj[j]) {
      i <- i + 1
      j <- length(obj) + 1 - i
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
}





# 1 6
# 2 5
# 3 4 

# bar[left] == bar[right]





