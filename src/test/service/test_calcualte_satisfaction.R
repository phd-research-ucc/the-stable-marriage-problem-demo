# Meta Data --------------------------------------------------------------------
#
# Version:      1.0
# Author:       Oleksii Dovhaniuk
# Created on:   2024-01-31
# Updated on:   2024-01-31
#
# Description:  Testing the implementation of the calculate_satisfaction function.
#
# Location:     src/test/service/test_calcualte_satisfaction.R
#


# Setup ------------------------------------------------------------------------


library(testthat)




# Test Two Pairs |M| = |W| -----------------------------------------------------


actual_result_01 = calculate_satisfaction(
  matrix(c(1, 2, 1, 2), nrow = 2, byrow = TRUE),
  matrix(c(1, 2, 1, 2), nrow = 2, byrow = TRUE),
  c(1, 2)
)

actual_result_02 = calculate_satisfaction(
  matrix(c(2, 1, 2, 1), nrow = 2, byrow = TRUE),
  matrix(c(1, 2, 2, 1), nrow = 2, byrow = TRUE),
  c(1, 2)
)

actual_result_03T = calculate_satisfaction(
  matrix(c(2, 1, 1, 2), nrow = 2, byrow = TRUE),
  matrix(c(1, 2, 1, 2), nrow = 2, byrow = TRUE),
  c(2, 1)
)

actual_result_03F = calculate_satisfaction(
  matrix(c(2, 1, 1, 2), nrow = 2, byrow = TRUE),
  matrix(c(1, 2, 1, 2), nrow = 2, byrow = TRUE),
  c(1, 2)
)

test_that(
  ' A basic test of the calculate satisfaction function - two pairs.', 
  {
    expect_equal(actual_result_01, 2)
    expect_equal(actual_result_02, 3)
    expect_equal(actual_result_03T, 3)
    expect_equal(actual_result_03F, 1)
  }
)




# Test Three Pairs |M| = |W| -----------------------------------------------------


actual_result_01T = calculate_satisfaction(
  matrix(c(1, 2, 3, 1, 2, 3, 1, 2, 3), nrow = 3, byrow = TRUE),
  matrix(c(1, 2, 3, 1, 2, 3, 1, 2, 3), nrow = 3, byrow = TRUE),
  c(1, 2, 3)
)

actual_result_01F = calculate_satisfaction(
  matrix(c(1, 2, 3, 1, 2, 3, 1, 2, 3), nrow = 3, byrow = TRUE),
  matrix(c(1, 2, 3, 1, 2, 3, 1, 2, 3), nrow = 3, byrow = TRUE),
  c(3, 2, 1)
)

actual_result_02 = calculate_satisfaction(
  matrix(c(3, 2, 1, 3, 2, 1, 3, 2, 1), nrow = 3, byrow = TRUE),
  matrix(c(3, 2, 1, 3, 2, 1, 3, 2, 1), nrow = 3, byrow = TRUE),
  c(1, 2, 3)
)

actual_result_03 = calculate_satisfaction(
  matrix(c(1, 2, 3, 1, 3, 2, 2, 1, 3), nrow = 3, byrow = TRUE),
  matrix(c(3, 2, 1, 3, 2, 1, 3, 2, 1), nrow = 3, byrow = TRUE),
  c(3, 1, 2)
)

test_that(
  ' A basic test of the calculate satisfaction function - three pairs.', 
  {
    expect_equal(actual_result_01T, 6)
    expect_equal(actual_result_01F, 6)
    expect_equal(actual_result_02, 6)
    expect_equal(actual_result_03, 5)
  }
)

