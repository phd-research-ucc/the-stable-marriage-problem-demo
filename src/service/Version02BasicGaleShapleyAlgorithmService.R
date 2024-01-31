# Meta Data --------------------------------------------------------------------
#
# Version:      2.0
# Author:       Oleksii Dovhaniuk
# Created on:   2024-01-24
# Updated on:   2024-01-28
#
# Description:  Service that implements the basic Gale-Shapley Algorithm
#               which returns the stable matching and the metrics for the
#               particular run of the algorithm such as:
#                 - Average Duration of One Cycle (d);
#                 - Number of Cycles (n);
#                 - Number of Rejections (Re);
#                 - Number of Pare Changes (Ch);
#                 - Number of Proposals (Pr);
#               
#
# Location:     service/Version02BasicGaleShapleyAlgorithm.R
#


# Setup ------------------------------------------------------------------------



library(tidyverse)


# Functions --------------------------------------------------------------------



# Function to check if a woman is already engaged to a better man
is_better_option <- function(woman, current_man, women_matched, women_preferences) {
  engaged_man <- women_matched[woman]
  engaged_man_rank <- which(women_preferences[woman, ] == engaged_man)
  current_man_rank <- which(women_preferences[woman, ] == current_man)
  # print(paste0('COMPARE: ', current_man, '[', current_man_rank, '] V ', engaged_man, '[', engaged_man_rank, ']'))
  if (current_man_rank < engaged_man_rank) {
    return(engaged_man)
  }
  return(FALSE)
}



arrange_stable_marriages <- function(men_preferences, women_preferences) {
  n <- nrow(men_preferences)
  
  # Initialise arrays to store matching information
  men_matched <- rep(NA, n)
  women_matched <- rep(NA, n)
  
  # Initialise arrays to store proposed women
  proposed <- matrix( rep(NA, length(men_preferences)), nrow = n)
  
  counter <- 0
  Re <- 0
  Ch <- 0
  Pr <- 0
  start_time <- Sys.time()
  # Main loop of the Gale-Shapley algorithm
  while ( any( is.na(men_matched) ) ) {
    counter <- counter + 1
    # Find the first non-engaged man
    current_man <- which( is.na(men_matched) )[1]
    
    # Find the woman he prefers the most among those he hasn't proposed to
    preferred_woman_index <- which( is.na(proposed[current_man, ]) )[1]
    preferred_woman_index <- ifelse(is.na(preferred_woman_index), 1, preferred_woman_index)
    preferred_woman <- men_preferences[current_man, preferred_woman_index]
    proposed[current_man, preferred_woman_index] <- TRUE
    Pr <- Pr + 1
    
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
      Ch <- Ch + 1
      next
    }
    Re <- Re + 1
    
  }
  end_time <- Sys.time()
  d <- (end_time - start_time) / counter 
  # Return the final matching with the solution metric.
  return( list(men_matched, counter, d, Re, Ch, Pr) )
}




# Tests ---------------------------------------------------------------


source('test/service/test_stable_marriage_algorithm_with_metrics.R')
