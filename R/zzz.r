
library(futile.logger)

.onLoad <- function(libname, pkgname) {
  log_level <- Sys.getenv("PPRL_LOG_LEVEL", "INFO")
  recognized_levels <- c("TRACE", "DEBUG", "INFO", "WARN", "ERROR", "FATAL")
  if (!log_level %in% recognized_levels) {
    warning(sprintf("Unrecognized level '%s'. Defaulting to INFO.", log_level))
    log_level <- "INFO"
  }

  # 3. Set the flog threshold
  futile.logger::flog.threshold(log_level, name = "ROOT")
  futile.logger::flog.info("Initializing PPRL with log level %s", log_level)
}