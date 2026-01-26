# ------------------------------------------------------------
# Helper: extract CO2, CH4, N2O peak heights from a single .txt
# ------------------------------------------------------------
# ---- logging helpers ----
.gc_log_env <- new.env(parent = emptyenv())
.gc_log_env$indent <- 0L
.gc_log_env$verbose <- TRUE
.gc_log_env$collect <- FALSE
.gc_log_env$buffer <- list()

gc_log_setup <- function(verbose = TRUE, collect = FALSE) {
  .gc_log_env$verbose <- isTRUE(verbose)
  .gc_log_env$collect <- isTRUE(collect)
  .gc_log_env$buffer <- list()
  invisible(TRUE)
}

gc_log_indent <- function(delta) {
  .gc_log_env$indent <- max(0L, .gc_log_env$indent + as.integer(delta))
  invisible(.gc_log_env$indent)
}

log_msg <- function(level = "INFO", ..., always = FALSE) {
  # only print INFO/DEBUG when verbose; always print WARN/ERROR or when always=TRUE
  lvl <- toupper(level)
  should_print <- always || lvl %in% c("WARN", "ERROR") || isTRUE(.gc_log_env$verbose)
  
  msg <- paste(..., collapse = " ")
  line <- sprintf("[%s] %s%s %s",
                  format(Sys.time(), "%H:%M:%S"),
                  strrep("  ", .gc_log_env$indent),
                  sprintf("%-5s", lvl),
                  msg)
  
  if (isTRUE(.gc_log_env$collect)) {
    .gc_log_env$buffer[[length(.gc_log_env$buffer) + 1]] <- line
  }
  
  if (should_print) cat(line, "\n")
  invisible(line)
}

log_run_header <- function(run_dir, run_label, folder_date) {
  log_msg("INFO", "========================================", always = TRUE)
  log_msg("INFO", "RUN:", run_label, " | folder:", basename(run_dir), " | date:", as.character(folder_date), always = TRUE)
  log_msg("INFO", "Path:", run_dir, always = TRUE)
  log_msg("INFO", "----------------------------------------", always = TRUE)
}

log_run_footer <- function(ok = TRUE, ...) {
  if (ok) {
    log_msg("INFO", "DONE:", ..., always = TRUE)
  } else {
    log_msg("ERROR", "FAILED:", ..., always = TRUE)
  }
  log_msg("INFO", "========================================", always = TRUE)
}

write_run_log <- function(path) {
  if (!isTRUE(.gc_log_env$collect)) return(invisible(FALSE))
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeLines(unlist(.gc_log_env$buffer), path)
  invisible(TRUE)
}