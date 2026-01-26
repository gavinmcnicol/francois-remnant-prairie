run_step02_all_dates <- function(tidy_root = "flux-processing/data/tidy", overwrite = FALSE) {
  peak_files <- list.files(
    tidy_root,
    pattern = "_gc_peak_heights\\.csv$",
    full.names = TRUE,
    recursive = TRUE,
    date_filter
  )
  
  if (!is.null(date_filter)) {
    date_filter <- as.character(date_filter)
    height_files <- height_files[stringr::str_detect(height_files, paste0("/", date_filter, "/"))]
    # or if date is only in basename:
    # height_files <- height_files[stringr::str_detect(basename(height_files), paste0("^", date_filter))]
  }
  
  purrr::walk(peak_files, \(pf) {
    run_label <- stringr::str_extract(basename(pf), "^\\d{6}")
    out_file  <- file.path(dirname(pf), paste0(run_label, "_ghg_ppm.csv"))
    
    if (!overwrite && file.exists(out_file)) {
      log_msg("SKIP (exists):", out_file)
      return(invisible(NULL))
    }
    
    heights_to_ppm(pf, out_file = out_file)
  })
  
  invisible(TRUE)
}