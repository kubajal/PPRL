#!/usr/bin/env Rscript
if (!requireNamespace("testthat", quietly = TRUE)) {
  install.packages("testthat")
}

library(testthat)

# Include the core logic
source("pprl.r")

# -------------------------------------------------------------------
# Unit Tests
# -------------------------------------------------------------------

test_that("generate_hashed_substrings_with_offsets handles empty and short input", {
  expect_equal(generate_hashed_substrings_with_offsets(""), "")
  expect_equal(generate_hashed_substrings_with_offsets("a"), "")
  expect_equal(generate_hashed_substrings_with_offsets("ab"), "")
})

test_that("generate_hashed_substrings_with_offsets handles exact-length input", {
  text <- "abc"
  result <- generate_hashed_substrings_with_offsets(text)
  expect_true(nchar(result) > 0, info = "Result should not be empty for exact-length input")
  expect_match(result, "_1_", info = "The result should contain position information")
})

test_that("generate_hashed_substrings_with_offsets handles longer text input", {
  text <- "abcdefgh"
  result <- generate_hashed_substrings_with_offsets(text)
  substr_count <- nchar(text) - 2
  expect_equal(length(strsplit(result, "\\$")[[1]]), substr_count,
               info = "Result should contain a hash for each substring of length 3")
})

test_that("generate_hashed_substrings_with_offsets includes correct salts and positions", {
  text <- "abcdef"
  result <- generate_hashed_substrings_with_offsets(text)

  hashes <- strsplit(result, "\\$")[[1]]
  for (hash_entry in hashes) {
    parts <- strsplit(hash_entry, "_")[[1]]
    expect_equal(length(parts), 3, info = "Each hash entry should have three parts (hash, position, salt)")
    expect_match(parts[2], "^[0-9]+$", info = "The second part of the entry should be a position")
    expect_true(parts[3] %in% precomputed_salts, info = "The salt should be a valid precomputed salt")
  }
})

test_that("precompute_salts generates unique salts", {
  salts <- precompute_salts(10, 12)
  expect_equal(length(unique(salts)), length(salts), info = "Salts should be unique")
  expect_equal(nchar(salts[1]), 12, info = "Each salt should have the correct length")
})

test_that("hash_with_salt_and_offset produces consistent results for identical input", {
  text <- "xyz"
  first_result <- generate_hashed_substrings_with_offsets(text)
  second_result <- generate_hashed_substrings_with_offsets(text)

  expect_equal(first_result, second_result, info = "Hashing should produce consistent results for identical input")
})

test_that("generate_hashed_substrings_with_offsets handles offset boundaries correctly", {
  text <- "abcd"
  result <- generate_hashed_substrings_with_offsets(text)
  hashes <- strsplit(result, "\\$")[[1]]

  for (hash_entry in hashes) {
    parts <- strsplit(hash_entry, "_")[[1]]
    position <- as.numeric(parts[2])
    expect_true(position >= 0 && position <= MAX_POSITION, info = "Position should be within valid range")
  }
})
