# Meta Data ---------------------------------------------------------------
#
# Version:      1.0
# Author:       Oleksii Dovhaniuk
# Created on:   2024-01-29
# Updated on:   2024-01-31
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
  rep(NA, times = N * SAMPLE_SIZE), 
  dim = c(N, SAMPLE_SIZE)
)
source('service/Version01StabilityCheckingAlgorithmService.R')
array_of_checks <- array(
  rep(NA, times = SAMPLE_SIZE), 
  dim = c(SAMPLE_SIZE)
)

for (sample in 1:SAMPLE_SIZE) {
  men_preferences <- array_of_inputs[, , 1, sample]
  women_preferences <- array_of_inputs[, , 2, sample]
  array_of_matches[ , sample] <- arrange_stable_marriages(
    men_preferences,
    women_preferences
  )
  array_of_checks[sample] <- check_stability(
    men_preferences,
    women_preferences,
    array_of_matches[ , sample]
  ) 
}





source('service/Version02BasicGaleShapleyAlgorithmService.R')
array_of_matches <- array(
  rep(NA, times = N * SAMPLE_SIZE), 
  dim = c(N, SAMPLE_SIZE)
)
source('service/Version01StabilityCheckingAlgorithmService.R')
array_of_checks <- array(
  rep(NA, times = SAMPLE_SIZE), 
  dim = c(SAMPLE_SIZE)
)
results_collection_2_pairs <- data.frame(
  count = c(),
  pairs = c(),
  hashed_preferences = c(),
  hashed_matching = c(),
  process_time = c(),
  cycles = c(),
  cycle_duration = c(),
  proposals = c(),
  rejections = c(),
  changes = c(),
  satisfaction = c(),
  men_optimal = c(),
  women_optimal = c(),
  satisfaction_optimal = c(),
  correct = c()
)

for (sample in 1:SAMPLE_SIZE) {
  men_preferences <- array_of_inputs[, , 1, sample]
  women_preferences <- array_of_inputs[, , 2, sample]
  start_time <- Sys.time()
  result <- arrange_stable_marriages(
    men_preferences,
    women_preferences
  )
  end_time <- Sys.time()
  hashed_matching <- digest(
    serialize(result[[1]], NULL),
    algo = "md5"
  )
  is_stable <- check_stability(
    men_preferences,
    women_preferences,
    result[[1]]
  ) 
  result_row <- data.frame(
    count = sample,
    pairs = N,
    hashed_preferences = array_of_hashes[sample],
    hashed_matching = hashed_matching,
    process_time = end_time - start_time,
    cycles = result[[2]],
    cycle_duration = result[[3]],
    proposals = result[[6]],
    rejections = result[[4]],
    changes = result[[5]],
    satisfaction = NA,
    men_optimal = TRUE,
    women_optimal = NA,
    satisfaction_optimal = NA,
    stable = is_stable
  )

  results_collection_2_pairs <- rbind(results_collection_2_pairs, result_row)
}

View(results_collection_2_pairs)

results_collection <- rbind(results_collection_2_pairs, results_collection_3_pairs)

View(results_collection)

save(
  results_collection, 
  file = glue('data/exp/{Sys.Date()}_results_collection.RData')
)
