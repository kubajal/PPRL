
# Global constants for configuration
config = list()
config$MAX_POSITION <- as.integer(Sys.getenv("PPRL_MAX_POSITION", unset = "1000"))
config$OFFSET_RANGE <- as.integer(Sys.getenv("PPRL_OFFSET_RANGE", unset = "3"))
config$SALT_LENGTH <- as.integer(Sys.getenv("PPRL_SALT_LENGTH", unset = "32"))
config$SUBSTRING_LENGTH <- as.integer(Sys.getenv("PPRL_SUBSTRING_LENGTH", unset = "2"))
config$LOG_LEVEL <- as.character(Sys.getenv("PPRL_LOG_LEVEL", unset = "INFO"))
config$COLUMN_WIDTH <- as.integer(Sys.getenv("PPRL_COLUMN_WIDTH", unset = 10))
