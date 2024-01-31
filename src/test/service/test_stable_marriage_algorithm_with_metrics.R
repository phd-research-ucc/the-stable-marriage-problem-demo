# Meta Data --------------------------------------------------------------------
#
# Version:      1.0
# Author:       Oleksii Dovhaniuk
# Created on:   2024-01-24
# Updated on:   2024-01-26
#
# Description:  Testing the basic Gale Shapley algorithm implementation
#
# Location:     test/service/test_stable_marriage_algorithm.R
#


# Setup ------------------------------------------------------------------------



library(testthat)


# Test Two Pairs |M| = |W| -----------------------------------------------------


actual_result_01 = arrange_stable_marriages(
  matrix(c(1, 2, 1, 2), nrow = 2, byrow = TRUE),
  matrix(c(1, 2, 1, 2), nrow = 2, byrow = TRUE)
)[[1]]
expect_result_01 = 1:2

actual_result_02 = arrange_stable_marriages(
  matrix(c(2, 1, 2, 1), nrow = 2, byrow = TRUE),
  matrix(c(1, 2, 2, 1), nrow = 2, byrow = TRUE)
)[[1]]
expect_result_02 = 1:2

actual_result_03 = arrange_stable_marriages(
  matrix(c(2, 1, 1, 2), nrow = 2, byrow = TRUE),
  matrix(c(1, 2, 1, 2), nrow = 2, byrow = TRUE)
)[[1]]
expect_result_03 = 2:1

test_that(
  ' A basic test of the stable marriage algorithm - two pairs.', 
  {
    expect_equal(actual_result_01, expect_result_01)
    expect_equal(actual_result_02, expect_result_02)
    expect_equal(actual_result_03, expect_result_03)
  }
)




# Test Three Pairs |M| = |W| -----------------------------------------------------


actual_result_01 = arrange_stable_marriages(
  matrix(c(1, 2, 3, 1, 2, 3, 1, 2, 3), nrow = 3, byrow = TRUE),
  matrix(c(1, 2, 3, 1, 2, 3, 1, 2, 3), nrow = 3, byrow = TRUE)
)[[1]]
expect_result_01 = 1:3

actual_result_02 = arrange_stable_marriages(
  matrix(c(3, 2, 1, 3, 2, 1, 3, 2, 1), nrow = 3, byrow = TRUE),
  matrix(c(3, 2, 1, 3, 2, 1, 3, 2, 1), nrow = 3, byrow = TRUE)
)[[1]]
expect_result_02 = 1:3

actual_result_03 = arrange_stable_marriages(
  matrix(c(1, 2, 3, 1, 3, 2, 2, 1, 3), nrow = 3, byrow = TRUE),
  matrix(c(3, 2, 1, 3, 2, 1, 3, 2, 1), nrow = 3, byrow = TRUE)
)[[1]]
expect_result_03 = c(3, 1, 2)

test_that(
  ' A basic test of the stable marriage algorithm - two pairs.', 
  {
    expect_equal(actual_result_01, expect_result_01)
    expect_equal(actual_result_02, expect_result_02)
    expect_equal(actual_result_03, expect_result_03)
  }
)

