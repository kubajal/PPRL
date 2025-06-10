
if (!requireNamespace("digest", quietly = TRUE)) {
  stop("Package 'digest' is needed for hashing. Please install it.")
}

if (!requireNamespace("futile.logger", quietly = TRUE)) {
  stop("Package 'futile.logger' is needed for hashing. Please install it.")
}

library(digest)
library(futile.logger)

flog.threshold(config$LOG_LEVEL)

futile.logger::flog.info("PPRL config:")
for (key in names(config)) {
  futile.logger::flog.info(paste0("  ", key, " : ", config[[key]]))
}

# Precompute salts for positions from -OFFSET_RANGE to (config$MAX_POSITION + config$OFFSET_RANGE)
precompute_salts <- function(num_positions, salt_length) {
  sapply(0:num_positions, function(x) {
    paste0(sample(c(letters, LETTERS, 0:9), salt_length, replace = TRUE), collapse = "")
  })
}

precomputed_salts <- precompute_salts(config$MAX_POSITION + config$OFFSET_RANGE, config$SALT_LENGTH)
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
    results <- sapply(-config$OFFSET_RANGE:config$OFFSET_RANGE, function(offset) {
      pos <- position + offset
      if (pos >= 0 && pos <= config$MAX_POSITION) {
        salt <- precomputed_salts[[pos+1]]
        salted_input <- paste0(salt, substring, pos)
        hash <- digest(salted_input, algo = "sha512")
        hash_with_position <- paste0(pos, "_", salt, "_", hash)
        futile.logger::flog.debug("  @ position=%s offset=%s salt=%s hash_with_position=%s", position, offset, salt, hash_with_position)
        hash_with_position
      } else {
        NA
      }
    })
    hashes <- results[!is.na(results)]
    futile.logger::flog.debug("substring=%s: joining %i hashes by '$'", substring, length(hashes))
    paste(hashes, collapse = "$")
  }

  # Generate and hash substrings with offsets and salts
  substrings <- sapply(1:n, function(i) {
    substring <- substr(text, i, i + config$SUBSTRING_LENGTH)
    hash_with_salt_and_offset(substring, i)
  })

  paste(substrings, collapse = "$")
}

calculate_fraction <- function(vector_a, vector_b) {
  unique_vector_a <- unique(vector_a)
  unique_vector_b <- unique(vector_b)
  unique_all <- unique(c(vector_a, vector_b))
  matching_count <- sum(unique_vector_a %in% unique_vector_b)
  result <- matching_count / (length(unique_all))
  result
}
