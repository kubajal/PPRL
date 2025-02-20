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
  expect_true(nchar(result) > 0, info = "Result should not be empty")
  expect_match(result, "1_", info = "The result should contain position")
})

test_that("generate_hashed_substrings handles longer text input", {
  text <- "abcdefgh"
  result <- generate_hashed_substrings(text)
  substr_count <- nchar(text) - 2
  hashes <- strsplit(result, "\\$")[[1]]
  expect_equal(length(hashes), substr_count,
               info = "Result should contain a hash for each 3-long substring")
})

test_that("generate_hashed_substrings includes correct salts and positions", {
  text <- "abcdef"
  result <- generate_hashed_substrings(text)

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
  first_result <- generate_hashed_substrings(text)
  second_result <- generate_hashed_substrings(text)

  expect_equal(first_result, second_result, info = "Hashing should produce consistent results for identical input")
})

test_that("generate_hashed_substrings handles offset boundaries correctly", {
  text <- "abcd"
  result <- generate_hashed_substrings(text)
  hashes <- strsplit(result, "\\$")[[1]]

  for (hash_entry in hashes) {
    parts <- strsplit(hash_entry, "_")[[1]]
    position <- as.numeric(parts[2])
    expect_true(position >= 0 && position <= MAX_POSITION, info = "Position should be within valid range")
  }
})
