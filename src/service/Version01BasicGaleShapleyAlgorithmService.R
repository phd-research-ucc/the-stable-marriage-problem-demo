# Meta Data --------------------------------------------------------------------
#
# Version:      1.0
# Author:       Oleksii Dovhaniuk
# Created on:   2024-01-24
# Updated on:   2024-01-24
#
# Description:  Service that implements the basic Gale-Shapley Algorithm
#
# Location:     service/Version01BasicGaleShapleyAlgorithm.R
#


# Setup ------------------------------------------------------------------------



library(tidyverse)


# Functions --------------------------------------------------------------------



# Function to check if a woman is already engaged to a better man
is_better_option <- function(woman, current_man, women_matched, women_preferences) {
  engaged_man <- women_matched[woman]
  engaged_man_rank <- which(women_preferences[woman, ] == engaged_man)
  current_man_rank <- which(women_preferences[woman, ] == current_man)
  print(paste0('COMPARE: ', current_man, '[', current_man_rank, '] V ', engaged_man, '[', engaged_man_rank, ']'))
  if (current_man_rank < engaged_man_rank) {
    return(engaged_man)
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
    preferred_woman_index <- which( is.na(proposed[current_man, ]) )[1]
    preferred_woman_index <- ifelse(is.na(preferred_woman_index), 1, preferred_woman_index)
    preferred_woman <- men_preferences[current_man, preferred_woman_index]
    proposed[current_man, preferred_woman_index] <- TRUE
    
    print(paste0('match: ', current_man, '-', preferred_woman))
    print(paste0('m: ', men_matched))
    print(paste0('w: ', women_matched))
    print(proposed)
    print(paste0('counter: ', counter))
    
    if ( is.na(women_matched[preferred_woman]) ) {
      women_matched[preferred_woman] <- current_man
      men_matched[current_man] <- preferred_woman
      next
    }
    
    better_option <- is_better_option(preferred_woman, current_man, women_matched, women_preferences)
    if ( better_option != FALSE ) {
      women_matched[preferred_woman] <- current_man
      men_matched[current_man] <- preferred_woman
      men_matched[better_option] <- NA
      next
    }
    
    if (counter == n*n) {
      break
    }
    
  }
  # Return the final matching
  return(men_matched)
}

# Example usage
men_preferences <- matrix(c(2, 3, 1, 1, 3, 2, 3, 2, 1), nrow = 3, byrow = TRUE)
women_preferences <- matrix(c(1, 2, 3, 2, 3, 1, 1, 2, 3), nrow = 3, byrow = TRUE)

result <- gale_shapley(men_preferences, women_preferences)
print(result)
