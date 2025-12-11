# ============================================================
# GC batch processing: extract peak heights + compute ppm
# ============================================================

library(tidyverse)
library(lubridate)
library(janitor)
library(stringr)
library(readr)
library(dplyr)

# ------------------------------------------------------------
# Helper: extract CO2, CH4, N2O peak heights from a single .txt
# ------------------------------------------------------------
log_msg <- function(...) {
  cat(sprintf("[%s] %s\n",
              format(Sys.time(), "%H:%M:%S"),
              paste(..., collapse = " ")))
}

get_peak_heights_from_file <- function(path) {
  log_msg("Reading file:", path)
  lines <- readr::read_lines(path)
  
  # Find all Peak Table headers (these define where "Height" lives)
  peak_header_idx <- which(stringr::str_detect(lines, "^Peak#\\t"))
  if (length(peak_header_idx) == 0) {
    log_msg("  No 'Peak#' header found; returning NAs.")
    return(tibble::tibble(
      file       = basename(path),
      co2_height = NA_real_,
      ch4_height = NA_real_,
      n2o_height = NA_real_
    ))
  }
  
  get_height <- function(gas) {
    # all lines that mention this gas name (e.g. "CH4", "CO2", "N2O")
    gas_lines <- which(stringr::str_detect(lines, paste0("\\t", gas, "\\b")))
    if (length(gas_lines) == 0) {
      log_msg("  ", gas, ": no line with that gas name; returning NA.")
      return(NA_real_)
    }
    
    # loop through candidate lines and try to pair each with the nearest Peak# header above it
    for (idx in gas_lines) {
      hdrs <- peak_header_idx[peak_header_idx < idx]
      if (!length(hdrs)) next
      
      hi <- max(hdrs)  # nearest header above this line
      header_fields <- strsplit(lines[hi], "\t")[[1]]
      height_col <- which(header_fields == "Height")
      if (!length(height_col)) next
      
      height_col <- height_col[1]
      fields <- strsplit(lines[idx], "\t")[[1]]
      if (length(fields) < height_col) next
      
      val <- suppressWarnings(as.numeric(fields[height_col]))
      log_msg("  ", gas, ": using line", idx, "header", hi, "height", val)
      return(val)
    }
    
    log_msg("  ", gas, ": could not match to a Peak Table header; returning NA.")
    NA_real_
  }
  
  res <- tibble::tibble(
    file       = basename(path),
    co2_height = get_height("CO2"),
    ch4_height = get_height("CH4"),
    n2o_height = get_height("N2O")
  )
  
  log_msg("Finished file:", basename(path),
          "CO2:", res$co2_height,
          "CH4:", res$ch4_height,
          "N2O:", res$n2o_height)
  
  res
}

# ------------------------------------------------------------
# Process a single GC run directory:
#   e.g. data/fluxes/gc-raw/2023/3.22.23
# ------------------------------------------------------------
process_gc_run <- function(run_dir,
                           tidy_root = "flux-processing/data/tidy") {
  
  run_name <- basename(run_dir)              # e.g. "7.9.22"
  log_msg("→ Processing run folder:", run_dir)
  
  # Parse "7.9.22" → Date (assumes m.d.yy or m.dd.yy)
  gc_run_date <- lubridate::mdy(stringr::str_replace_all(run_name, "\\.", "/"))
  log_msg("  Parsed gc_run_date as:", as.character(gc_run_date))
  
  if (is.na(gc_run_date)) {
    log_msg("  ERROR: Could not parse run folder name to date. Skipping.")
    return(invisible(NULL))
  }
  
  run_label <- format(gc_run_date, "%y%m%d")   # e.g. 220709
  log_msg("  Using run_label:", run_label)
  
  # ---- sample log ----
  sample_log_path <- file.path(run_dir, "sample_log.csv")
  if (!file.exists(sample_log_path)) {
    log_msg("  WARNING: sample_log.csv not found at", sample_log_path, "– skipping run.")
    return(invisible(NULL))
  }
  
  log_msg("  Reading sample log:", sample_log_path)
  sample_log <- readr::read_csv(
    sample_log_path,
    na = c("N/a", "<NA>"),
    show_col_types = FALSE
  ) %>%
    janitor::clean_names()
  
  log_msg("  Sample log raw columns:", paste(names(sample_log), collapse = ", "))
  
  # --- normalize column names across different log formats ---
  
  # 1) chamber column: allow things like "chamber", "chamber_no", "chamber_number"
  nm <- names(sample_log)
  chamber_col <- nm[grepl("^chamber", nm)]
  if (!length(chamber_col)) {
    stop("No 'chamber' column found in sample_log for run folder: ", run_dir)
  }
  if (chamber_col[1] != "chamber") {
    sample_log <- sample_log %>%
      dplyr::rename(chamber = dplyr::all_of(chamber_col[1]))
  }
  
  # 2) timepoint column: allow "timepoint" or "time_point"
  if (!"timepoint" %in% names(sample_log)) {
    tp_col <- nm[grepl("^time_?point$", nm)]
    if (length(tp_col)) {
      sample_log <- sample_log %>%
        dplyr::rename(timepoint = dplyr::all_of(tp_col[1]))
    }
  }
  
  # 3) date_collected: if present as text, parse it to Date
  if ("date_collected" %in% names(sample_log)) {
    sample_log <- sample_log %>%
      dplyr::mutate(
        date_collected = suppressWarnings(
          lubridate::mdy(date_collected)
        )
      )
  }
  
  # 4) ensure we have filename and derive numeric row from it
  if (!"filename" %in% names(sample_log)) {
    stop("No 'filename' column found in sample_log for run folder: ", run_dir)
  }
  
  sample_log <- sample_log %>%
    dplyr::mutate(
      gc_run_date = gc_run_date,  # Date object from folder name
      # pull the numeric part after the underscore, e.g. "220909_000.txt" -> 0
      row = readr::parse_number(stringr::str_replace(filename, "\\.txt$", ""))
    )
  
  log_msg("  Sample log rows:", nrow(sample_log))
  log_msg("  Sample log rows:", nrow(sample_log))
  
  # ---- GC text files ----
  txt_files <- list.files(run_dir,
                          pattern = "\\.txt$",
                          full.names = TRUE)
  log_msg("  Found", length(txt_files), ".txt files in", run_dir)
  if (length(txt_files) == 0) {
    log_msg("  WARNING: No .txt files in this run folder – skipping.")
    return(invisible(NULL))
  }
  
  # Extract peaks (calls your working get_peak_heights_from_file)
  peak_heights <- purrr::map_dfr(txt_files, get_peak_heights_from_file) %>%
    mutate(row = dplyr::row_number()) %>%
    dplyr::select(row, co2_height, ch4_height, n2o_height)
  log_msg("  Peak heights table rows:", nrow(peak_heights))
  
  # ---- join with sample log ----
  gc_data_tidy <- dplyr::left_join(peak_heights, sample_log, by = "row") %>%
    mutate(chamber = dplyr::if_else(is.na(chamber), "STD", chamber)) %>%
    dplyr::select(
      row,
      date_collected,
      timepoint,
      gc_run_date,
      filename,
      chamber,
      co2_height,
      ch4_height,
      n2o_height
    )
  log_msg("  gc_data_tidy rows after join:", nrow(gc_data_tidy))
  
  # ---- output directory + write peak heights ----
  out_dir <- file.path(tidy_root, run_label)
  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    log_msg("  Created output directory:", out_dir)
  } else {
    log_msg("  Output directory already exists:", out_dir)
  }
  
  peaks_outfile <- file.path(out_dir,
                             paste0(run_label, "_gc_peak_heights.csv"))
  readr::write_csv(gc_data_tidy, peaks_outfile)
  log_msg("  ✓ Peak heights written:", peaks_outfile)
  
  # ---- std stats across all runs ----
  std_heights <- gc_data_tidy %>% 
    dplyr::filter(
      chamber == "STD",
      !is.na(gc_run_date),
      n2o_height > 10000,
      n2o_height < 15000,
      row != 1
    ) %>% 
    dplyr::group_by(gc_run_date) %>% 
    dplyr::summarize(
      mean_co2_height = mean(co2_height, na.rm = TRUE),
      mean_ch4_height = mean(ch4_height, na.rm = TRUE),
      mean_n2o_height = mean(n2o_height, na.rm = TRUE),
      co2_precision   = sd(co2_height, na.rm = TRUE) / mean_co2_height * 100,
      ch4_precision   = sd(ch4_height, na.rm = TRUE) / mean_ch4_height * 100,
      n2o_precision   = sd(n2o_height, na.rm = TRUE) / mean_n2o_height * 100,
      .groups         = "drop"
    )
  
  # ---- combine std heights with unknowns ----
  unknown_std_heights <- gc_data_tidy %>% 
    dplyr::filter(chamber != "STD") %>% 
    dplyr::left_join(std_heights, by = "gc_run_date")
  
  # ---- calculate GHG in ppm ----
  ghg_ppm <- unknown_std_heights %>% 
    dplyr::mutate(
      co2_ppm = co2_height * (998  / mean_co2_height),
      ch4_ppm = ch4_height * (10.2 / mean_ch4_height),
      n2o_ppm = n2o_height * (1    / mean_n2o_height)
    )
  
  # ---- write ppm data ----
  ppm_outfile <- file.path(out_dir,
                           paste0(run_label, "_ghg_ppm.csv"))
  readr::write_csv(ghg_ppm, ppm_outfile)
  log_msg("  ✓ GHG ppm written:", ppm_outfile)
  
  log_msg("✔ Finished run:", run_dir)
  invisible(gc_data_tidy)
}

# ------------------------------------------------------------
# Top-level: process ALL years/dates under data/fluxes/gc-raw
# ------------------------------------------------------------
run_all_gc <- function(
    base_dir  = "data/fluxes/gc-raw",
    tidy_root = "data/tidy"
) {
  
  year_dirs <- list.dirs(base_dir, recursive = FALSE, full.names = TRUE)
  
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