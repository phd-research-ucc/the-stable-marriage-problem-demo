# Meta Data --------------------------------------------------------------------
#
# Version:      1.0
# Author:       Oleksii Dovhaniuk
# Created on:   2024-01-24
# Updated on:   2024-01-24
#
# Description:  Service that implements the basic Gale-Shapley Algorithm
#
# Location:     service/BasicGaleShapleyAlgorithm.R
#


# Setup ------------------------------------------------------------------------



library(tidyverse)


# Functions --------------------------------------------------------------------



# Function to check if a woman is already engaged to a better man
is_better_option <- function(woman, current_man, women_matched, women_preferences) {
  engaged_man <- women_matched[woman]
  engaged_man_rank <- which(women_preferences[woman, ] == engaged_man)
  current_man_rank <- which(women_preferences[woman, ] == current_man)
  
  if (current_man_rank < engaged_man_rank) {
    return(TRUE)
  }
    return(FALSE)
}


gale_shapley <- function(men_preferences, women_preferences) {
  n <- nrow(men_preferences)
  
  # Initialize arrays to store matching information
  men_matched <- rep(NA, n)
  women_matched <- rep(NA, n)
  
  # initialise arrays to store proposed women
  proposed <- matrix( rep(NA, length(men_preferences)), nrow = n)
  
  counter <- 0
  # Main loop of the Gale-Shapley algorithm
  while ( any( is.na(men_matched) ) ) {
    counter <- counter + 1
    # Find the first unengaged man
    current_man <- which( is.na(men_matched) )[1]
    
    # Find the woman he prefers the most among those he hasn't proposed to
    preferred_woman <- which( is.na(proposed[current_man, ]) )[1]
    preferred_woman <- ifelse(is.na(preferred_woman), 1, preferred_woman)
    proposed[current_man, preferred_woman] <- TRUE
    
    print(paste0('match: ', current_man, '-', preferred_woman))
    print(men_matched)
    print(proposed)
    print(paste0('counter: ', counter))
    
    if ( is.na(women_matched[preferred_woman]) ) {
      women_matched[preferred_woman] <- current_man
      men_matched[current_man] <- preferred_woman
      next
    }
    
    if ( is_better_option(preferred_woman, current_man, women_matched, women_preferences) ) {
      women_matched[preferred_woman] <- current_man
      men_matched[current_man] <- preferred_woman
      next
    }
    
    if (counter > n*n){
      break
    }
  }
  # Return the final matching
  return(men_matched)
}

# Example usage
men_preferences <- matrix(c(1, 2, 3, 1, 2, 3, 1, 2, 3), nrow = 3, byrow = TRUE)
women_preferences <- matrix(c(1, 2, 3, 1, 2, 3, 1, 2, 3), nrow = 3, byrow = TRUE)

result <- gale_shapley(men_preferences, women_preferences)
print(result)
