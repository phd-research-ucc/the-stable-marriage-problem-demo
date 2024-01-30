# Meta Data ---------------------------------------------------------------
#
# Version:      1.0
# Author:       Oleksii Dovhaniuk
# Created on:   2024-01-29
# Updated on:   2024-01-30
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

N <- 3
SAMPLE_SIZE <- 10

# Generate all permutations
all_permutations <- permn( c(1:N) )

# Convert the result to a matrix
permutations_matrix <- matrix(
  unlist(all_permutations),
  ncol = N,
  byrow = TRUE
)
num_permutations <- nrow(permutations_matrix)

# Print the matrix
print(permutations_matrix)


array_of_inputs <- array(
  rep(NA, times = N * N * 2 * SAMPLE_SIZE), 
  dim = c(N, N, 2, SAMPLE_SIZE)
)
print(array_of_inputs)
array_of_hashes <- array(
  rep(NA, times = SAMPLE_SIZE), 
  dim = c(SAMPLE_SIZE)
)
print(array_of_hashes)

input_size <- N * N * 2
sample <- 1
while (sample <= SAMPLE_SIZE) {
  inputs <- array(
    rep(NA, times = input_size), 
    dim = c(N, N, 2)
  )
  
  # Men preferences
  for (i in 1:N){
    sample_preferences <- sample(1:num_permutations, 1)
    inputs[i, , 1] <- permutations_matrix[sample_preferences, ]
  }
  
  # Women preferences
  for (i in 1:N){
    sample_preferences <- sample(1:num_permutations, 1)
    inputs[i, , 2] <- permutations_matrix[sample_preferences, ]
  }
  
  # Check for duplicates
  input_hash <- digest(
    serialize(inputs, NULL),
    algo = "md5"
  )
  if ( !(input_hash %in% array_of_hashes) ) {
    array_of_inputs[, , , sample] <- inputs
    array_of_hashes[sample] <- input_hash
    sample <- sample + 1
  }
}

print(array_of_hashes)
print(array_of_inputs[, , , 2])


source('service/Version01BasicGaleShapleyAlgorithmService.R')
array_of_matches <- array(
  rep(NA, times = SAMPLE_SIZE), 
  dim = c(N, SAMPLE_SIZE)
)

for (sample in 1:SAMPLE_SIZE) {
  men_preferences <- array_of_inputs[, , 1, sample]
  women_preferences <- array_of_inputs[, , 2, sample]
  array_of_matches[ , sample] <- arrange_stable_marriages(
    men_preferences,
    women_preferences
  )
}





