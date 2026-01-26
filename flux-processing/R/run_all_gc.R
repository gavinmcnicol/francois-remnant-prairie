run_all_gc <- function(
    base_dir  = "data/fluxes/gc-raw",
    tidy_root = "data/tidy",
    date_filter = NULL
) {
  year_dirs <- list.dirs(base_dir, recursive = FALSE, full.names = TRUE)
  
  # Get all .txt files under year dirs
  gc_files <- list.files(
    path = year_dirs,
    pattern = "\\.txt$",
    full.names = TRUE,
    recursive = TRUE
  )
  
  # Optional date filtering
  if (!is.null(date_filter)) {
    date_filter <- as.character(date_filter)
    
    gc_files <- gc_files[
      vapply(gc_files, function(f) {
        d <- tryCatch(extract_gc_run_date_from_txt(f), error = function(e) NA_character_)
        identical(d, date_filter)
      }, logical(1))
    ]
  }
  
  # Process each file (placeholder - fill in your actual logic)
  for (f in gc_files) {
    message("Processing: ", f)
    # your processing function here
  }

  
  if (length(gc_files) == 0) {
    stop("run_all_gc(): no GC files matched date_filter = ", date_filter)
  }
  log_msg("GC files to process:", length(gc_files))
  
  for (yd in year_dirs) {
    year_name <- basename(yd)
    message("==== Year: ", year_name, " ====")
    
    # date dirs like "3.22.23" or "10.05.23"
    date_dirs <- list.dirs(yd, recursive = FALSE, full.names = TRUE)
    date_dirs <- date_dirs[
      grepl("\\d{1,2}\\.\\d{1,2}\\.\\d{2}$", basename(date_dirs))
    ]
    
    for (dd in date_dirs) {
      try(process_gc_run(dd, tidy_root = tidy_root), silent = FALSE)
    }
  }
  
  invisible(TRUE)
}