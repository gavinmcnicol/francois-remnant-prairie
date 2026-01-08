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
  
  gc_data_tidy <- readr::read_csv(peaks_file, show_col_types = FALSE) %>%
    mutate(
      gc_run_date = as.Date(suppressWarnings(
        lubridate::parse_date_time(
          as.character(gc_run_date),
          orders = c("mdy", "m/d/y", "ymd", "y-m-d")
        )
      )),
      co2_height = suppressWarnings(as.numeric(co2_height)),
      ch4_height = suppressWarnings(as.numeric(ch4_height)),
      n2o_height = suppressWarnings(as.numeric(n2o_height))
    )
  
  log_msg("Columns in peak heights:", paste(names(gc_data_tidy), collapse = ", "))
  
  if (!"chamber" %in% names(gc_data_tidy)) stop("No 'chamber' column in ", peaks_file)
  
  gc_data_tidy <- gc_data_tidy %>%
    mutate(
      chamber_str = as.character(chamber),
      site_str    = if ("site" %in% names(.)) as.character(site) else NA_character_,
      is_std      = str_detect(chamber_str, regex("std", ignore_case = TRUE)) |
        str_detect(site_str,    regex("std", ignore_case = TRUE))
    )
  
  log_msg("Chamber counts:")
  print(count(gc_data_tidy, chamber_str, is_std))
  
  log_msg("gc_run_date non-NA rows:", sum(!is.na(gc_data_tidy$gc_run_date)), "/", nrow(gc_data_tidy))
  
  # ------------------------------------------------------------
  # Add low-peak QC flags (applies to all rows; we use it for STDs)
  # ------------------------------------------------------------
  gc_data_tidy <- gc_data_tidy %>%
    mutate(
      qc_co2_low = is_std & !is.na(co2_height) & co2_height < 900000,
      qc_ch4_low = is_std & !is.na(ch4_height) & ch4_height < 8750,
      qc_n2o_low = is_std & !is.na(n2o_height) & n2o_height < 12500,
      qc_any_low = qc_co2_low | qc_ch4_low | qc_n2o_low
    )
  
  # ------------------------------------------------------------
  # 1) Standard stats (EXCLUDE low-peak STD rows from means / SDs)
  # ------------------------------------------------------------
  std_input <- gc_data_tidy %>%
    filter(is_std, !is.na(gc_run_date)) %>%
    filter(!qc_any_low)   # <--- KEY LINE: drop bad injections
  
  std_heights <- std_input %>%
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
  
  # Counts after exclusion (what you *actually* used to compute means)
  std_counts <- std_input %>%
    count(gc_run_date, name = "n_std_used")
  
  # Optional: total STD count before exclusion (nice for diagnostics)
  std_counts_raw <- gc_data_tidy %>%
    filter(is_std, !is.na(gc_run_date)) %>%
    count(gc_run_date, name = "n_std_total")
  
  std_heights <- std_heights %>%
    left_join(std_counts, by = "gc_run_date") %>%
    left_join(std_counts_raw, by = "gc_run_date") %>%
    mutate(
      n_std_dropped_low = n_std_total - n_std_used
    )
  
  # Your existing qc_flag logic, but use n_std_used not n_std
  std_heights <- std_heights %>%
    mutate(
      qc_flag = case_when(
        n_std_used < 3 ~ "INSUFFICIENT_STD",
        co2_precision > 2 | ch4_precision > 2 | n2o_precision > 5 ~ "WARN",
        TRUE ~ "OK"
      )
    )
  
  log_msg("STD dropped (low peaks) total:",
          sum(std_heights$n_std_dropped_low, na.rm = TRUE))
  
  # 2) Unknowns
  unknown_std_heights <- gc_data_tidy %>%
    filter(!is_std, !is.na(gc_run_date)) %>%
    left_join(std_heights, by = "gc_run_date") %>%
    filter(qc_flag %in% c("OK", "WARN"))   # or just "OK" if strict
  
  log_msg("Unknown (non-STD) rows:", nrow(unknown_std_heights))
  if (nrow(unknown_std_heights) == 0) {
    log_msg("NOTE: No non-STD rows (or gc_run_date is NA for them).")
  }
  
  # 3) ppm conversion
  ghg_ppm <- unknown_std_heights %>%
    mutate(
      co2_ppm = co2_height * (998  / mean_co2_height),
      ch4_ppm = ch4_height * (10.2 / mean_ch4_height),
      n2o_ppm = n2o_height * (1    / mean_n2o_height)
    )
  
  # 4) Output paths
  dir  <- dirname(peaks_file)
  base <- basename(peaks_file)
  base_run <- sub("_gc_peak_heights\\.csv$", "", base)
  
  if (is.null(out_file)) out_file <- file.path(dir, paste0(base_run, "_ghg_ppm.csv"))
  std_outfile <- file.path(dir, paste0(base_run, "_std_heights.csv"))
  
  readr::write_csv(std_heights, std_outfile)
  log_msg("✓ Wrote std_heights to:", std_outfile)
  
  std_rows_used <- std_input %>%
    select(gc_run_date, filename, co2_height, ch4_height, n2o_height)
  readr::write_csv(std_rows_used, file.path(dir, paste0(base_run, "_std_rows_used.csv")))
  
  if (nrow(ghg_ppm) == 0) {
    log_msg("NOTE: No non-STD samples found; not writing ghg_ppm.csv for this run.")
    return(invisible(list(std_heights = std_heights, ghg_ppm = ghg_ppm)))
  }
  
  readr::write_csv(ghg_ppm, out_file)
  log_msg("✓ Wrote ghg_ppm to:", out_file)
  
  invisible(list(std_heights = std_heights, ghg_ppm = ghg_ppm))
  
  ghg_ppm <- ghg_ppm %>%
    select(-chamber_str, -site_str, -is_std)
  
}