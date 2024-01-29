# Meta Data --------------------------------------------------------------------
#
# Version:      1.0
# Author:       Oleksii Dovhaniuk
# Created on:   2024-01-24
# Updated on:   2024-01-25
#
# Description:  Service that implements the basic stability checking
#               algorithm
#
# Location:     service/Version01StabilityCheckingAlgorithmService.R
#


# Setup ------------------------------------------------------------------------



library(tidyverse)


# Functions --------------------------------------------------------------------

# Function to check if there is a blocking pair
has_blocking_pair <- function(man_index, woman_index, matching, men_preferences, women_preferences) {
  
  current_woman <- matching[man]
  current_man <- which(matching == woman)[1]

  man_index <- which(women_preferences[woman, ] == man)
  current_man_index <- which(women_preferences[woman, ] == current_man)

  woman_index <- which(men_preferences[man, ] == woman)
  current_woman_index <- which(men_preferences[man, ] == current_woman)

  return(man_index < current_man_index && woman_index < current_woman_index)
}

check_stability <- function(men_preferences, women_preferences, matching) {

  n <- nrow(men_preferences)
  
  # Check for blocking pairs in the matching
  for (m in 1:n) {
    woman <- matching[m]
    for (w in 1:which(man_preferences[m, ] == woman) ){
      if (has_blocking_pair(m, w, matching, men_preferences, women_preferences)) {
        return(FALSE)  # Matching is not stable
      }
    }
  }
  
  return(TRUE)  # Matching is stable
}

# Example usage
# men_preferences <- matrix(c(1, 2, 3, 1, 2, 3, 1, 2, 3), nrow = 3, byrow = TRUE)
# women_preferences <- matrix(c(1, 2, 3, 1, 2, 3, 1, 2, 3), nrow = 3, byrow = TRUE)

# matching <- c(1, 2, 3)
# is_stable <- check_stability(men_preferences, women_preferences, matching)
# print(is_stable)



source('test/service/test_stability_checking_algorithm.R')



