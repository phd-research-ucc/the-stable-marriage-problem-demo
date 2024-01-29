# Meta Data --------------------------------------------------------------------
#
# Version:      1.0
# Author:       Oleksii Dovhaniuk
# Created on:   2024-01-24
# Updated on:   2024-01-26
#
# Description:  Testing the stability checking algorithm implementation
#
# Location:     test/service/test_stability_checking_algorithm.R
#


# Setup ------------------------------------------------------------------------


library(testthat)




# Test Two Pairs |M| = |W| -----------------------------------------------------


actual_result_01T = check_stability(
  matrix(c(1, 2, 1, 2), nrow = 2, byrow = TRUE),
  matrix(c(1, 2, 1, 2), nrow = 2, byrow = TRUE),
  c(1, 2)
)

actual_result_01F = check_stability(
  matrix(c(1, 2, 1, 2), nrow = 2, byrow = TRUE),
  matrix(c(1, 2, 1, 2), nrow = 2, byrow = TRUE),
  c(2, 1)
)

actual_result_02T = check_stability(
  matrix(c(2, 1, 2, 1), nrow = 2, byrow = TRUE),
  matrix(c(1, 2, 2, 1), nrow = 2, byrow = TRUE),
  c(1, 2)
)

actual_result_02F = check_stability(
  matrix(c(2, 1, 2, 1), nrow = 2, byrow = TRUE),
  matrix(c(1, 2, 2, 1), nrow = 2, byrow = TRUE),
  c(2, 1)
)

actual_result_03T = check_stability(
  matrix(c(2, 1, 1, 2), nrow = 2, byrow = TRUE),
  matrix(c(1, 2, 1, 2), nrow = 2, byrow = TRUE),
  c(2, 1)
)

actual_result_03F = check_stability(
  matrix(c(2, 1, 1, 2), nrow = 2, byrow = TRUE),
  matrix(c(1, 2, 1, 2), nrow = 2, byrow = TRUE),
  c(1, 2)
)

test_that(
  ' A basic test of the stable marriage algorithm - two pairs.', 
  {
    expect_equal(actual_result_01T, TRUE)
    expect_equal(actual_result_01F, FALSE)
    expect_equal(actual_result_02T, TRUE)
    expect_equal(actual_result_02F, FALSE)
    expect_equal(actual_result_03T, TRUE)
    expect_equal(actual_result_03F, FALSE)
  }
)




# Test Three Pairs |M| = |W| -----------------------------------------------------


actual_result_01T = check_stability(
  matrix(c(1, 2, 3, 1, 2, 3, 1, 2, 3), nrow = 3, byrow = TRUE),
  matrix(c(1, 2, 3, 1, 2, 3, 1, 2, 3), nrow = 3, byrow = TRUE),
  c(1, 2, 3)
)

actual_result_01F = check_stability(
  matrix(c(1, 2, 3, 1, 2, 3, 1, 2, 3), nrow = 3, byrow = TRUE),
  matrix(c(1, 2, 3, 1, 2, 3, 1, 2, 3), nrow = 3, byrow = TRUE),
  c(3, 2, 1)
)

actual_result_02T = check_stability(
  matrix(c(3, 2, 1, 3, 2, 1, 3, 2, 1), nrow = 3, byrow = TRUE),
  matrix(c(3, 2, 1, 3, 2, 1, 3, 2, 1), nrow = 3, byrow = TRUE),
  c(1, 2, 3)
)

actual_result_02F = check_stability(
  matrix(c(3, 2, 1, 3, 2, 1, 3, 2, 1), nrow = 3, byrow = TRUE),
  matrix(c(3, 2, 1, 3, 2, 1, 3, 2, 1), nrow = 3, byrow = TRUE),
  c(3, 2, 1)
)

actual_result_03T = check_stability(
  matrix(c(1, 2, 3, 1, 3, 2, 2, 1, 3), nrow = 3, byrow = TRUE),
  matrix(c(3, 2, 1, 3, 2, 1, 3, 2, 1), nrow = 3, byrow = TRUE),
  c(3, 1, 2)
)

actual_result_03F = check_stability(
  matrix(c(1, 2, 3, 1, 3, 2, 2, 1, 3), nrow = 3, byrow = TRUE),
  matrix(c(3, 2, 1, 3, 2, 1, 3, 2, 1), nrow = 3, byrow = TRUE),
  c(1, 2, 3)
)

test_that(
  ' A basic test of the stable marriage algorithm - two pairs.', 
  {
    expect_equal(actual_result_01T, TRUE)
    expect_equal(actual_result_01F, FALSE)
    expect_equal(actual_result_02T, TRUE)
    expect_equal(actual_result_02F, FALSE)
    expect_equal(actual_result_03T, TRUE)
    expect_equal(actual_result_03F, FALSE)
  }
)

