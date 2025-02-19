
library(digest)

# Global constants for configuration
MAX_POSITION <- as.integer(Sys.getenv("PPRL_MAX_POSITION", unset = "1000"))
OFFSET_RANGE <- as.integer(Sys.getenv("PPRL_OFFSET_RANGE", unset = "3"))
SALT_LENGTH <- as.integer(Sys.getenv("PPRL_SALT_LENGTH", unset = "32"))

# Precompute salts for positions from -OFFSET_RANGE to (MAX_POSITION + OFFSET_RANGE)
precompute_salts <- function(num_positions, salt_length) {
  sapply(0:num_positions, function(x) {
    paste0(sample(c(letters, LETTERS, 0:9), salt_length, replace = TRUE), collapse = "")
  })
}

# Precompute salts
precomputed_salts <- precompute_salts(MAX_POSITION + OFFSET_RANGE, SALT_LENGTH)

# Function to generate substrings of length 3 with offsets and precomputed salts
generate_hashed_substrings_with_offsets <- function(text) {
  n <- nchar(text)
  if (n < 3) {
    return("")  # Return an empty string if the text is shorter than 3 characters
  }

  # Helper function to hash a substring with offset-based salt
  hash_with_salt_and_offset <- function(substring, position) {
    results <- sapply(-OFFSET_RANGE:OFFSET_RANGE, function(offset) {
      new_pos <- position + offset
      if (new_pos >= 0 && new_pos <= MAX_POSITION) {
        salt <- precomputed_salts[[new_pos+1]]
        salted_input <- paste0(salt, substring, new_pos)
        hash <- digest(salted_input, algo = "sha512")
        paste0(hash, "_", new_pos, "_", salt)
      } else {
        NA
      }
    })
    results <- results[!is.na(results)]  # Remove NA values for out-of-bound offsets
    paste(results, collapse = "$")
  }

  # Generate and hash substrings with offsets and salts
  substrings <- sapply(1:(n - 2), function(i) {
    substring <- substr(text, i, i + 2)
    hash_with_salt_and_offset(substring, i)
  })

  paste(substrings, collapse = "$")
}
