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
get_peak_heights_from_file <- function(path) {
  lines <- readr::read_lines(path)
  
  # internal helper: find Height for a given compound name
  get_height <- function(name_pattern) {
    # find the line containing e.g. "\tCO2\t" or "\tN2O\t"
    idx <- stringr::str_which(lines, paste0("\\t", name_pattern, "\\t"))
    if (length(idx) == 0) return(NA_real_)
    
    line_idx <- idx[1]
    
    # find the header "ID#   Name   R.Time   Area   Height ..."
    header_idx <- max(which(stringr::str_detect(lines[1:line_idx], "^ID#\\t")))
    header_fields <- strsplit(lines[header_idx], "\t")[[1]]
    height_col   <- which(header_fields == "Height")[1]
    
    fields <- strsplit(lines[line_idx], "\t")[[1]]
    as.numeric(fields[height_col])
  }
  
  tibble(
    file       = basename(path),
    co2_height = get_height("CO2"),
    ch4_height = get_height("CH4"),
    n2o_height = get_height("N2O")
  )
}

# ------------------------------------------------------------
# Process a single GC run directory:
#   e.g. data/fluxes/gc-raw/2023/3.22.23
# ------------------------------------------------------------
process_gc_run <- function(run_dir,
                           tidy_root = "data/tidy") {
  
  run_name <- basename(run_dir)              # e.g. "3.22.23"
  message("→ Processing run folder: ", run_name)
  
  # Convert "3.22.23" → date; assumes m.d.yy or m.dd.yy format
  gc_run_date <- mdy(str_replace_all(run_name, "\\.", "/"))
  run_label   <- format(gc_run_date, "%y%m%d")   # e.g. "230322"
  
  # ---- sample log ----
  sample_log_path <- file.path(run_dir, "sample_log.csv")
  if (!file.exists(sample_log_path)) {
    warning("  ! No sample_log.csv in ", run_dir, " – skipping.")
    return(invisible(NULL))
  }
  
  sample_log <- readr::read_csv(
    sample_log_path,
    na = c("N/a", "<NA>")
  ) %>%
    janitor::clean_names() %>%
    mutate(
      date_collected = mdy(substr(filename, 1, 6)),
      gc_run_date    = gc_run_date,
      row            = as.numeric(substr(filename, 8, 10))
    )
  
  # ---- GC text files ----
  txt_files <- list.files(run_dir,
                          pattern = "\\.txt$",
                          full.names = TRUE)
  if (length(txt_files) == 0) {
    warning("  ! No .txt files in ", run_dir, " – skipping.")
    return(invisible(NULL))
  }
  
  peak_heights <- purrr::map_dfr(txt_files, get_peak_heights_from_file) %>%
    mutate(row = row_number()) %>%
    select(row, co2_height, ch4_height, n2o_height)
  
  # ---- join peak heights with sample log ----
  gc_data_tidy <- left_join(peak_heights, sample_log, by = "row") %>%
    mutate(chamber = if_else(is.na(chamber), "STD", chamber)) %>%
    select(
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
  
  # ---- output directory + write peak heights ----
  out_dir <- file.path(tidy_root, run_label)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  
  peaks_outfile <- file.path(out_dir,
                             paste0(run_label, "_gc_peak_heights.csv"))
  readr::write_csv(gc_data_tidy, peaks_outfile)
  message("  ✓ Peak heights → ", peaks_outfile)
  
  # ---- std stats across all runs ----
  std_heights <- gc_data_tidy %>%
    filter(
      chamber == "STD",
      !is.na(gc_run_date),
      n2o_height > 10000,
      n2o_height < 15000,
      row != 1
    ) %>%
    group_by(gc_run_date) %>%
    summarize(
      mean_co2_height = mean(co2_height, na.rm = TRUE),
      mean_ch4_height = mean(ch4_height, na.rm = TRUE),
      mean_n2o_height = mean(n2o_height, na.rm = TRUE),
      
      co2_precision   = sd(co2_height, na.rm = TRUE) / mean_co2_height * 100,
      ch4_precision   = sd(ch4_height, na.rm = TRUE) / mean_ch4_height * 100,
      n2o_precision   = sd(n2o_height, na.rm = TRUE) / mean_n2o_height * 100,
      .groups = "drop"
    )
  
  # ---- combine std heights with unknowns + compute ppm ----
  unknown_std_heights <- gc_data_tidy %>%
    filter(chamber != "STD") %>%
    left_join(std_heights, by = "gc_run_date")
  
  ghg_ppm <- unknown_std_heights %>%
    mutate(
      co2_ppm = co2_height * (998  / mean_co2_height),
      ch4_ppm = ch4_height * (10.2 / mean_ch4_height),
      n2o_ppm = n2o_height * (1    / mean_n2o_height)
    )
  
  ppm_outfile <- file.path(out_dir,
                           paste0(run_label, "_ghg_ppm.csv"))
  readr::write_csv(ghg_ppm, ppm_outfile)
  message("  ✓ GHG ppm → ", ppm_outfile)
  
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