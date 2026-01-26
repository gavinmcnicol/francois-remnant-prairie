################
# Heights to ppm
################

heights_to_ppm <- function(peaks_file, out_file = NULL) {
  
  log_msg("Reading peak heights:", peaks_file)
  
  gc_data_tidy <- readr::read_csv(peaks_file, show_col_types = FALSE) %>%
    dplyr::mutate(
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
  
  # ensure these exist (some runs might not have them)
  if (!"site" %in% names(gc_data_tidy))      gc_data_tidy$site <- NA_character_
  if (!"ecosystem" %in% names(gc_data_tidy)) gc_data_tidy$ecosystem <- NA_character_
  if (!"timepoint" %in% names(gc_data_tidy)) gc_data_tidy$timepoint <- NA_character_
  
  std_tokens <- c("STD", "STANDARD")
  
  gc_data_tidy <- gc_data_tidy %>%
    dplyr::mutate(
      site_str    = stringr::str_to_upper(stringr::str_trim(as.character(site))),
      eco_str     = stringr::str_to_upper(stringr::str_trim(as.character(ecosystem))),
      tp_str      = stringr::str_to_upper(stringr::str_trim(as.character(timepoint))),
      chamber_str = stringr::str_to_upper(stringr::str_trim(as.character(chamber))),
      
      # IMPORTANT: coalesce to "" so is_std is NEVER NA
      is_std =
        (dplyr::coalesce(site_str,    "") %in% std_tokens) |
        (dplyr::coalesce(eco_str,     "") %in% std_tokens) |
        (dplyr::coalesce(tp_str,      "") %in% std_tokens) |
        (dplyr::coalesce(chamber_str, "") %in% std_tokens)
    )
  
  log_msg("STD vs non-STD rows:", sum(gc_data_tidy$is_std, na.rm = TRUE), "/", nrow(gc_data_tidy))
  log_msg("gc_run_date non-NA rows:", sum(!is.na(gc_data_tidy$gc_run_date)), "/", nrow(gc_data_tidy))
  
  # ------------------------------------------------------------
  # Low-peak QC flags (apply only to standards)
  # ------------------------------------------------------------
  gc_data_tidy <- gc_data_tidy %>%
    dplyr::mutate(
      qc_co2_low = is_std & !is.na(co2_height) & co2_height < 900000,
      qc_ch4_low = is_std & !is.na(ch4_height) & ch4_height < 8750,
      qc_n2o_low = is_std & !is.na(n2o_height) & n2o_height < 12500,
      qc_any_low = qc_co2_low | qc_ch4_low
    )
  
  # ------------------------------------------------------------
  # 1) Standard stats (EXCLUDE low-peak STD rows from means / SDs)
  # ------------------------------------------------------------
  std_all <- gc_data_tidy %>%
    dplyr::filter(is_std == TRUE, !is.na(gc_run_date))
  
  std_input <- std_all %>%
    dplyr::filter(qc_any_low == FALSE)   # drop bad injections
  
  # counts
  std_counts_raw <- std_all %>%
    dplyr::count(gc_run_date, name = "n_std_total")
  
  std_counts_used <- std_input %>%
    dplyr::count(gc_run_date, name = "n_std_used")
  
  log_msg("STD total rows:", nrow(std_all))
  log_msg("STD usable rows after low-peak QC:", nrow(std_input))
  
  # if no usable standards, write diagnostics and bail early (prevents silent NA joins)
  if (nrow(std_input) == 0) {
    log_msg("WARNING: No usable standards after low-peak QC; skipping ppm conversion for this run.")
    
    # still write a std_heights file (empty but with columns) for consistency
    std_heights <- tibble::tibble(
      gc_run_date = as.Date(character()),
      mean_co2_height = numeric(),
      mean_ch4_height = numeric(),
      mean_n2o_height = numeric(),
      co2_precision = numeric(),
      ch4_precision = numeric(),
      n2o_precision = numeric(),
      n_std_used = integer(),
      n_std_total = integer(),
      n_std_dropped_low = integer(),
      qc_flag = character()
    )
    
    dir  <- dirname(peaks_file)
    base <- basename(peaks_file)
    base_run <- sub("_gc_peak_heights\\.csv$", "", base)
    std_outfile <- file.path(dir, paste0(base_run, "_std_heights.csv"))
    readr::write_csv(std_heights, std_outfile)
    log_msg("✓ Wrote std_heights to:", std_outfile)
    
    # also write what we *would* have used (empty)
    readr::write_csv(std_input, file.path(dir, paste0(base_run, "_std_rows_used.csv")))
    return(invisible(list(std_heights = std_heights, ghg_ppm = tibble::tibble())))
  }
  
  std_heights <- std_input %>%
    dplyr::group_by(gc_run_date) %>%
    dplyr::summarize(
      mean_co2_height = mean(co2_height, na.rm = TRUE),
      mean_ch4_height = mean(ch4_height, na.rm = TRUE),
      mean_n2o_height = mean(n2o_height, na.rm = TRUE),
      co2_precision   = sd(co2_height, na.rm = TRUE) / mean_co2_height * 100,
      ch4_precision   = sd(ch4_height, na.rm = TRUE) / mean_ch4_height * 100,
      n2o_precision   = sd(n2o_height, na.rm = TRUE) / mean_n2o_height * 100,
      .groups = "drop"
    ) %>%
    dplyr::left_join(std_counts_used, by = "gc_run_date") %>%
    dplyr::left_join(std_counts_raw,  by = "gc_run_date") %>%
    dplyr::mutate(
      n_std_used = dplyr::coalesce(n_std_used, 0L),
      n_std_total = dplyr::coalesce(n_std_total, 0L),
      n_std_dropped_low = n_std_total - n_std_used,
      qc_flag = dplyr::case_when(
        n_std_used < 3 ~ "INSUFFICIENT_STD",
        co2_precision > 2 | ch4_precision > 2 | n2o_precision > 5 ~ "WARN",
        TRUE ~ "OK"
      )
    )
  
  log_msg("STD dropped (low peaks) total:", sum(std_heights$n_std_dropped_low, na.rm = TRUE))
  
  # ------------------------------------------------------------
  # 2) Unknowns (use is_std == FALSE to avoid NA-dropping)
  # ------------------------------------------------------------
  log_msg("Raw non-STD rows (before join):", sum(gc_data_tidy$is_std == FALSE & !is.na(gc_data_tidy$gc_run_date)))
  log_msg("std_heights qc_flag table:"); print(count(std_heights, qc_flag))
  
  unknown_std_heights <- gc_data_tidy %>%
    dplyr::filter(is_std == FALSE, !is.na(gc_run_date)) %>%
    dplyr::left_join(std_heights, by = "gc_run_date") %>%
    dplyr::filter(qc_flag %in% c("OK", "WARN"))
  
  log_msg("Unknown (non-STD) rows:", nrow(unknown_std_heights))
  
  # ------------------------------------------------------------
  # 3) ppm conversion
  # ------------------------------------------------------------
  ghg_ppm <- unknown_std_heights %>%
    dplyr::mutate(
      co2_ppm = co2_height * (998  / mean_co2_height),
      ch4_ppm = ch4_height * (10.1 / mean_ch4_height),
      n2o_ppm = n2o_height * (1   / mean_n2o_height)
    )
  
  # ------------------------------------------------------------
  # 4) Output paths + writes
  # ------------------------------------------------------------
  dir  <- dirname(peaks_file)
  base <- basename(peaks_file)
  base_run <- sub("_gc_peak_heights\\.csv$", "", base)
  
  if (is.null(out_file)) out_file <- file.path(dir, paste0(base_run, "_ghg_ppm.csv"))
  std_outfile <- file.path(dir, paste0(base_run, "_std_heights.csv"))
  
  readr::write_csv(std_heights, std_outfile)
  log_msg("✓ Wrote std_heights to:", std_outfile)
  
  # Write the STD rows actually used (for auditing)
  std_rows_used <- std_input %>%
    dplyr::select(gc_run_date, filename, co2_height, ch4_height, n2o_height, qc_any_low)
  readr::write_csv(std_rows_used, file.path(dir, paste0(base_run, "_std_rows_used.csv")))
  log_msg("✓ Wrote std_rows_used to:", file.path(dir, paste0(base_run, "_std_rows_used.csv")))
  
  if (nrow(ghg_ppm) == 0) {
    log_msg("NOTE: No non-STD samples found (or all filtered); not writing ghg_ppm.csv for this run.")
    return(invisible(list(std_heights = std_heights, ghg_ppm = ghg_ppm)))
  }
  
  readr::write_csv(ghg_ppm, out_file)
  log_msg("✓ Wrote ghg_ppm to:", out_file)
  
  invisible(list(std_heights = std_heights, ghg_ppm = ghg_ppm))
}