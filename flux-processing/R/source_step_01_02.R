source_step_01_02 <- function(script_dir = "R") {
  # Order matters: helpers first, then functions that depend on them
  src <- function(f) source(file.path(script_dir, f), local = FALSE)
  
  src("logging_helpers.R")
  src("extract_gc_run_date_from_txt.R")
  src("normalize_gc_filename_one.R")
  src("get_peak_heights_from_file.R")
  src("process_gc_run.R")
  src("run_all_gc.R")
  src("heights_to_ppm.R")
  src("run_step02_all_dates.R")
  
  invisible(TRUE)
}