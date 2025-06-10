#!/usr/bin/env Rscript

library(PPRL)
library(testthat)
library(futile.logger)
library(digest)

# -------------------------------------------------------------------
# Unit Tests
# -------------------------------------------------------------------

test_that("generate_hashed_substrings handles empty and short input", {
  expect_equal(generate_hashed_substrings(""), "")
  expect_equal(generate_hashed_substrings("a"), "")
  expect_equal(generate_hashed_substrings("ab"), "")
})

test_that("generate_hashed_substrings handles exact-length input", {
  text <- "abc"
  result <- generate_hashed_substrings(text)
  hashes <- strsplit(result, "\\$")[[1]]
  expect_equal(18, length(hashes), info = "Result should be 18 = 5 (abc) + 6 (bc) + 7 (c)")
})

test_that("generate_hashed_substrings handles longer text input", {
  text <- "abcdefgh"
  result <- generate_hashed_substrings(text)
  hashes <- strsplit(result, "\\$")[[1]]
  expect_equal(length(hashes), 53,
               info = "Result should be 5 (abc) + 6 (bcd) + 7 (def) + 7 (efg) + 7 (fgh) + 7 (gh) + 7 (h)")
})

test_that("precompute_salts generates unique salts", {
  salts <- precompute_salts(10, 12)
  expect_equal(length(unique(salts)), length(salts), info = "Salts should be unique")
  expect_equal(nchar(salts[1]), 12, info = "Each salt should have the correct length")
})

test_that("hash_with_salt_and_offset produces consistent results for identical input", {
  text <- "xyz"
  first_result <- generate_hashed_substrings(text)
  second_result <- generate_hashed_substrings(text)

  expect_equal(first_result, second_result, info = "Hashing should produce consistent results for identical input")
})

test_that("hash_with_salt_and_offset produces consistent results for identical input", {
  text <- "xyz"
  first_result <- generate_hashed_substrings(text)
  second_result <- generate_hashed_substrings(text)

  expect_equal(first_result, second_result, info = "Hashing should produce consistent results for identical input")
})

test_that("hash_with_salt_and_offset produces consistent results for identical input", {
  text <- "xyz"
  first_result <- generate_hashed_substrings(text)
  second_result <- generate_hashed_substrings(text)

  expect_equal(first_result, second_result, info = "Hashing should produce consistent results for identical input")
})

test_that("calculate_fraction works for unique values", {
  vector_a <- c("a", "b", "c", "d", "e")
  vector_b <- c("b", "c", "d", "e", "f")
  fraction <- calculate_fraction(vector_a, vector_b)
  expect_equal(fraction, 2/3, info = "calculate_fraction should return correct fraction when values are unique")
})

test_that("calculate_fraction works for non-unique values 1", {
  vector_a <- c("a", "a", "b", "c", "d", "e")
  vector_b <- c("b", "c", "d", "e", "f")
  fraction <- calculate_fraction(vector_a, vector_b)
  expect_equal(fraction, 2/3, info = "calculate_fraction should return correct fraction when values are non-unique")
})

test_that("calculate_fraction works for non-unique values 2", {
  vector_a <- c("a", "b", "c", "d", "e")
  vector_b <- c("b", "b", "c", "d", "e", "f")
  fraction <- calculate_fraction(vector_a, vector_b)
  expect_equal(fraction, 2/3, info = "calculate_fraction should return correct fraction when values are non-unique")
})

test_that("calculate_fraction works for non-unique values 3", {
  vector_a <- c("a", "a", "b", "c", "d", "e")
  vector_b <- c("b", "b", "b", "c", "d", "e", "f")
  fraction <- calculate_fraction(vector_a, vector_b)
  expect_equal(fraction, 2/3, info = "calculate_fraction should return correct fraction when values are non-unique")
})

test_that("calculate_fraction is 1 for the same person", {
  similiarity <- calculate_similarity("First_Name#Last_Name#01-01-1111", "First_Name#Last_Name#01-01-1111")
  expect_equal(similiarity, 1, info = "Similiarty for the same person should be 1")
})

test_that("calculate_fraction is close to 1 for similar persons", {
  similiarity <- calculate_similarity("FirstName#LastName#01-01-1111", "First_Name#LastName#01-01-1111")
  expect_gt(similiarity, 0.5)
})

test_that("calculate_fraction is 0 for completely different persons", {
  similiarity <- calculate_similarity("aaa#bbb#01-01-1111", "ccc#ddd#02-02-2222")
  expect_equal(similiarity, 0, info = "Similiarty for the completely different persons should be 0")
})
