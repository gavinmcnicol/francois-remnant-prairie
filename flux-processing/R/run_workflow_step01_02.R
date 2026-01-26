# R/run_workflow_step01_02.R
run_workflow_step01_02 <- function(
    script_dir = "R",
    raw_gc_dir,
    tidy_root,
    date_filter = NULL,     # "YYMMDD" or NULL
    overwrite = FALSE,
    verbose = TRUE
) {
  source_step01_02(script_dir = script_dir)
  
  # Step 1: extract heights
  # If your run_all_gc() already supports filtering, pass it through.
  # Otherwise, filter raw_gc_dir upstream (e.g., only files for that date).
  run_all_gc(
    raw_gc_dir = raw_gc_dir,
    tidy_root  = tidy_root,
    overwrite  = overwrite,
    verbose    = verbose,
    date_filter = date_filter
  )
  
  # Step 2: convert heights -> ppm (optionally filter by date)
  run_step02_all_dates(
    tidy_root   = tidy_root,
    overwrite   = overwrite,
    verbose     = verbose,
    date_filter = date_filter
  )
  
  invisible(TRUE)
}