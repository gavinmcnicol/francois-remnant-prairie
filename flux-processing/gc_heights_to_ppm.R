library(tidyverse)
library(dplyr)
library(readr)

log_msg <- function(...) {
  cat(sprintf("[%s] %s\n",
              format(Sys.time(), "%H:%M:%S"),
              paste(..., collapse = " ")))
}

heights_to_ppm <- function(peaks_file, out_file = NULL) {
  
  log_msg("Reading peak heights:", peaks_file)
  
  gc_data_tidy <- readr::read_csv(peaks_file, show_col_types = FALSE)
  
  log_msg("Columns in peak heights:",
          paste(names(gc_data_tidy), collapse = ", "))
  
  # ------------------------------------------------------------------
  # 1) Identify standards vs unknowns robustly
  # ------------------------------------------------------------------
  if (!"chamber" %in% names(gc_data_tidy)) {
    stop("No 'chamber' column in ", peaks_file)
  }
  
  gc_data_tidy <- gc_data_tidy %>%
    mutate(
      chamber_str = as.character(chamber),
      is_std      = str_detect(chamber_str, regex("std", ignore_case = TRUE))
    )
  
  log_msg("Chamber counts:")
  print(count(gc_data_tidy, chamber_str, is_std))
  
  # ------------------------------------------------------------------
  # 2) Standard stats (no N2O height filter initially)
  # ------------------------------------------------------------------
  std_heights <- gc_data_tidy %>%
    filter(is_std, !is.na(gc_run_date)) %>%
    group_by(gc_run_date) %>%
    summarize(
      mean_co2_height = mean(co2_height, na.rm = TRUE),
      mean_ch4_height = mean(ch4_height, na.rm = TRUE),
      mean_n2o_height = mean(n2o_height, na.rm = TRUE),
      
      co2_precision   = sd(co2_height, na.rm = TRUE) / mean_co2_height * 100,
      ch4_precision   = sd(ch4_height, na.rm = TRUE) / mean_ch4_height * 100,
      n2o_precision   = sd(n2o_height, na.rm = TRUE) / mean_n2o_height * 100,
      .groups         = "drop"
    )
  
  log_msg("STD rows used:", nrow(std_heights))
  
  if (nrow(std_heights) == 0) {
    log_msg("WARNING: No standards found (no rows where chamber contains 'std'). ghg_ppm will be empty.")
  }
  
  # ------------------------------------------------------------------
  # 3) Unknowns (non-STD) joined to std stats
  # ------------------------------------------------------------------
  unknown_std_heights <- gc_data_tidy %>%
    filter(!is_std, !is.na(gc_run_date)) %>%
    left_join(std_heights, by = "gc_run_date")
  
  log_msg("Unknown (non-STD) rows:", nrow(unknown_std_heights))
  
  if (nrow(unknown_std_heights) == 0) {
    log_msg("WARNING: No non-STD rows found. This run appears to contain only standards.")
  }
  
  # ------------------------------------------------------------------
  # 4) Calculate ppm (will be all NA if no stds)
  # ------------------------------------------------------------------
  ghg_ppm <- unknown_std_heights %>%
    mutate(
      co2_ppm = co2_height * (998  / mean_co2_height),
      ch4_ppm = ch4_height * (10.2 / mean_ch4_height),
      n2o_ppm = n2o_height * (1    / mean_n2o_height)
    )
  
  # ------------------------------------------------------------------
  # 5) Output path
  # ------------------------------------------------------------------
  if (is.null(out_file)) {
    dir  <- dirname(peaks_file)
    base <- basename(peaks_file)
    base_run <- sub("_gc_peak_heights\\.csv$", "", base)
    out_file <- file.path(dir, paste0(base_run, "_ghg_ppm.csv"))
  }
  
  readr::write_csv(ghg_ppm, out_file)
  log_msg("✓ Wrote ghg_ppm to:", out_file)
  
  invisible(list(std_heights = std_heights,
                 ghg_ppm     = ghg_ppm))
}

heights_to_ppm_all <- function(tidy_root = "flux-processing/data/tidy") {
  
  peak_files <- list.files(
    tidy_root,
    pattern = "_gc_peak_heights\\.csv$",
    full.names = TRUE,
    recursive = TRUE
  )
  
  if (!length(peak_files)) {
    log_msg("No *_gc_peak_heights.csv files found under", tidy_root)
    return(invisible(NULL))
  }
  
  log_msg("Found", length(peak_files), "peak-height files.")
  
  for (f in peak_files) {
    log_msg("----")
    heights_to_ppm(f)
  }
  
  invisible(TRUE)
}