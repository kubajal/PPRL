
if (!requireNamespace("digest", quietly = TRUE)) {
  stop("Package 'digest' is needed for hashing. Please install it.")
}

if (!requireNamespace("futile.logger", quietly = TRUE)) {
  stop("Package 'futile.logger' is needed for hashing. Please install it.")
}

library(digest)
library(futile.logger)

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

precomputed_salts <- precompute_salts(MAX_POSITION + OFFSET_RANGE, SALT_LENGTH)
futile.logger::flog.debug("Precomputed salts: %s", precomputed_salts)

generate_hashed_substrings <- function(text) {
  futile.logger::flog.debug("Generating hashed substrings for %s", text)
  n <- nchar(text)
  if (n < 3) {
    return("")  # Return an empty string if the text is shorter than 3ch
  }

  # Helper function to hash a substring with offset-based salt
  hash_with_salt_and_offset <- function(substring, position) {
    futile.logger::flog.debug("Got text for hashing: %s", substring)
    results <- sapply(-OFFSET_RANGE:OFFSET_RANGE, function(offset) {
      pos <- position + offset
      if (pos >= 0 && pos <= MAX_POSITION) {
        salt <- precomputed_salts[[pos+1]]
        salted_input <- paste0(salt, substring, pos)
        hash <- digest(salted_input, algo = "sha512")
        futile.logger::flog.debug("  @ pos=%s salt=%s hash=%s", pos, salt, hash)
        paste0(pos, "_", salt, "_", hash)
      } else {
        NA
      }
    })
    hashes <- results[!is.na(results)]
    futile.logger::flog.debug("Joining %i hashes by '$'", length(hashes))
    paste(hashes, collapse = "$")
  }

  # Generate and hash substrings with offsets and salts
  substrings <- sapply(1:(n - 2), function(i) {
    substring <- substr(text, i, i + 2)
    hash_with_salt_and_offset(substring, i)
  })

  paste(substrings, collapse = "$")
}
