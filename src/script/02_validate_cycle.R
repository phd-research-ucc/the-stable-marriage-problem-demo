# Meta Data ---------------------------------------------------------------
#
# Version:      1.0
# Author:       Oleksii Dovhaniuk
# Created on:   2024-01-29
# Updated on:   2024-01-29
#
# Description:  Validates the correctness of the matching algorithm
#               for N pairs
#
#
#
# Location:     script/02_validate_cycle.R
#


# Setup -------------------------------------------------------------------


source('script/_load_packages.R')


# Script ------------------------------------------------------------------

validate_cycle <- function(N = 2) {
  if (N < 2) {
    N <- 2
  }
  
}

N <- 2

# Generate all permutations
all_permutations <- permn(1:N)

# Convert the result to a matrix
permutations_matrix <- matrix(unlist(all_permutations), ncol = N, byrow = TRUE)

# Print the matrix
print(permutations_matrix)


decimal_sequence <- 0:15

# Function to convert decimal to binary vector
decimal_to_binary_vector <- function(decimal_number) {
  binary_raw <- intToBits(decimal_number)
  binary_vector <- as.integer(rev(binary_raw))[29:32] + 1
  return(binary_vector)
}

# Apply the function to each element in the sequence
binary_vectors_list <- lapply(decimal_sequence, decimal_to_binary_vector)

# Convert the list to a matrix
binary_matrix <- do.call(rbind, binary_vectors_list)

# Print the matrix
print(binary_matrix)


for (i in 1:length(binary_matrix)) {
  men_preferences <- list(
    permutations_matrix[binary_matrix[i, 1]], 
    permutations_matrix[binary_matrix[i, 2]]
  )
  women_preferences <- list(
    permutations_matrix[binary_matrix[i, 3]], 
    permutations_matrix[binary_matrix[i, 4]]
  )
  
  print(men_preferences)  
}


