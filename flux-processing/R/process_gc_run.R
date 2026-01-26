# ------------------------------------------------------------
# Process a single GC run directory:
#   e.g. data/fluxes/gc-raw/2023/3.22.23
# ------------------------------------------------------------
process_gc_run <- function(run_dir,
                           tidy_root = "flux-processing/data/tidy") {
  
  run_name <- basename(run_dir)              # e.g. "7.9.22"
  log_msg("→ Processing run folder:", run_dir)
  
  # Parse "7.9.22" → Date (assumes m.d.yy or m.dd.yy)
  folder_date <- lubridate::mdy(stringr::str_replace_all(run_name, "\\.", "/"))
  log_msg("  Parsed folder date as:", as.character(folder_date))
  
  if (is.na(folder_date)) {
    log_msg("  ERROR: Could not parse run folder name to date. Skipping.")
    return(invisible(NULL))
  }
  
  run_label <- format(folder_date, "%y%m%d")   # e.g. 220709
  log_msg("  Using run_label:", run_label)
  
  # Start clean header + reset log buffer per run (if collecting)
  gc_log_setup(verbose = .gc_log_env$verbose, collect = .gc_log_env$collect)
  log_run_header(run_dir, run_label, folder_date)
  gc_log_indent(+1)
  
  # ---- sample log ----
  sample_log_path <- file.path(run_dir, "sample_log.csv")
  if (!file.exists(sample_log_path)) {
    log_msg("  WARNING: sample_log.csv not found at", sample_log_path, "– skipping run.")
    return(invisible(NULL))
  }
  
  log_msg("  Reading sample log:", sample_log_path)
  sample_log <- readr::read_csv(
    sample_log_path,
    na = c("N/A", "N/a", "<NA>", ""),
    show_col_types = FALSE,
    col_types = readr::cols(.default = readr::col_character())
  ) %>%
    janitor::clean_names()
  
  # ---- drop malformed / non-sample rows ----
  sample_log <- sample_log %>%
    mutate(
      site      = na_if(stringr::str_trim(as.character(site)), ""),
      ecosystem = na_if(stringr::str_trim(as.character(ecosystem)), ""),
      time_point = na_if(stringr::str_trim(as.character(time_point)), "")
    ) %>%
    # drop fully blank rows
    filter(!(is.na(site) & is.na(ecosystem) & is.na(time_point))) %>%
    # drop STD rows that have no ecosystem/timepoint metadata (your 30-row bucket)
    filter(!(site == "STD" & is.na(ecosystem) & is.na(time_point)))
  
  log_msg("  Sample log columns after clean_names():", paste(names(sample_log), collapse = ", "))
  
  # ---- tolerant normalization of required columns ----
  nm <- names(sample_log)
  
  # filename (must exist in some form; try to map common variants)
  if (!"filename" %in% nm) {
    cand <- nm[nm %in% c("file_name", "file", "gc_file", "run_file")]
    if (length(cand) >= 1) {
      sample_log <- dplyr::rename(sample_log, filename = dplyr::all_of(cand[1]))
    } else {
      stop("sample_log.csv has no 'filename' column (or recognizable alias) in: ", sample_log_path)
    }
  }
  
  # date_collected
  nm <- names(sample_log)
  if (!"date_collected" %in% nm) {
    cand <- nm[nm %in% c("date", "date_sampled", "collection_date")]
    if (length(cand) >= 1) {
      sample_log <- dplyr::rename(sample_log, date_collected = dplyr::all_of(cand[1]))
    } else {
      sample_log$date_collected <- NA_character_
    }
  }
  
  # site
  nm <- names(sample_log)
  if (!"site" %in% nm) {
    cand <- nm[nm %in% c("site_", "plot", "location")]
    if (length(cand) >= 1) {
      sample_log <- dplyr::rename(sample_log, site = dplyr::all_of(cand[1]))
    } else {
      sample_log$site <- NA_character_
    }
  }
  
  # ecosystem
  nm <- names(sample_log)
  if (!"ecosystem" %in% nm) {
    cand <- nm[nm %in% c("eco", "habitat")]
    if (length(cand) >= 1) {
      sample_log <- dplyr::rename(sample_log, ecosystem = dplyr::all_of(cand[1]))
    } else {
      sample_log$ecosystem <- NA_character_
    }
  }
  
  # chamber_no
  nm <- names(sample_log)
  if (!"chamber_no" %in% nm) {
    cand <- nm[grepl("^chamber(_no|_number|_num)?$", nm) | nm %in% c("chamber", "chamber_id")]
    if (length(cand) >= 1) {
      sample_log <- dplyr::rename(sample_log, chamber_no = dplyr::all_of(cand[1]))
    } else {
      sample_log$chamber_no <- NA_real_
    }
  }
  
  # timepoint
  nm <- names(sample_log)
  if (!"timepoint" %in% nm) {
    cand <- nm[nm %in% c("time_point", "time_pt") | grepl("^time_?point$", nm)]
    if (length(cand) >= 1) {
      sample_log <- dplyr::rename(sample_log, timepoint = dplyr::all_of(cand[1]))
    } else {
      sample_log$timepoint <- NA_character_
    }
  }
  
  # gc_run_date (may be missing or blank; we will parse/fill)
  nm <- names(sample_log)
  if (!"gc_run_date" %in% nm) {
    cand <- nm[nm %in% c("gc_date", "run_date", "date_run")]
    if (length(cand) >= 1) {
      sample_log <- dplyr::rename(sample_log, gc_run_date = dplyr::all_of(cand[1]))
    } else {
      sample_log$gc_run_date <- NA_character_
    }
  }
  
  # ---- normalize values + types ----
  sample_log <- sample_log %>%
    dplyr::mutate(
      filename = stringr::str_trim(as.character(filename)),
      filename = stringr::str_replace(filename, "\\.txt$", ""),
      filename = stringr::str_replace(filename, "\\.gcd$", ""),
      
      site      = as.character(site),
      ecosystem = as.character(ecosystem),
      timepoint = as.character(timepoint),
      
      chamber_no = suppressWarnings(as.numeric(chamber_no)),
      
      date_collected = {
        dc <- str_trim(as.character(date_collected))
        dc <- na_if(dc, "")
        
        suppressWarnings(dplyr::case_when(
          stringr::str_detect(dc, "^\\d{8}$") & between(as.integer(substr(dc, 1, 4)), 1900L, 2100L) ~ lubridate::ymd(dc),                 # YYYYMMDD
          stringr::str_detect(dc, "^\\d{8}$")                                                     ~ as.Date(dc, format = "%m%d%Y"),       # MMDDYYYY
          TRUE ~ as.Date(lubridate::parse_date_time(dc, orders = c("mdy", "m/d/Y", "ymd", "Y-m-d")))
        ))
      },
      
      gc_run_date = as.Date(suppressWarnings(
        lubridate::parse_date_time(
          as.character(gc_run_date),
          orders = c("mdy", "m/d/y", "ymd", "y-m-d")
        )
      ))
    )
  
  sample_log <- sample_log %>%
    mutate(filename = normalize_gc_filename(filename))
  
  # derive canonical "chamber" label used downstream:
  # - numeric chamber_no -> "1"/"2"/...
  # - otherwise fall back to site (STD)
  sample_log <- sample_log %>%
    dplyr::mutate(
      chamber = dplyr::case_when(
        !is.na(chamber_no) ~ as.character(chamber_no),
        !is.na(site)       ~ as.character(site),
        TRUE               ~ NA_character_
      )
    )
  
  log_msg("  Chamber breakdown after normalization:")
  print(dplyr::count(sample_log, site, ecosystem, chamber_no, chamber))
  
  # ---- GC text files ----
  txt_files <- list.files(run_dir, pattern = "\\.txt$", full.names = TRUE)
  log_msg("  Found", length(txt_files), ".txt files in", run_dir)
  
  if (length(txt_files) == 0) {
    log_msg("  WARNING: No .txt files in this run folder – skipping.")
    return(invisible(NULL))
  }
  
  # ---- fill missing gc_run_date from GC txt files ----
  if (any(is.na(sample_log$gc_run_date))) {
    
    txt_files <- list.files(run_dir, pattern = "\\.txt$", full.names = TRUE)
    
    txt_dates <- tibble::tibble(
      filename_raw = tools::file_path_sans_ext(basename(txt_files)),
      gc_run_date_from_txt = purrr::map_chr(txt_files, \(p) {
        d <- extract_gc_run_date_from_txt(p)
        if (is.na(d)) NA_character_ else format(d, "%Y-%m-%d")
      }) |> as.Date()
    ) %>%
      mutate(filename = normalize_gc_filename(filename_raw)) %>%
      select(filename, gc_run_date_from_txt)
    
    sample_log <- sample_log %>%
      left_join(txt_dates, by = "filename") %>%
      mutate(gc_run_date = coalesce(gc_run_date, gc_run_date_from_txt)) %>%
      select(-gc_run_date_from_txt)
  }
  
  
  # Extract peaks
  peak_heights <- purrr::map_dfr(txt_files, \(p) {
    res <- get_peak_heights_from_file(p)
    dplyr::mutate(res, filename = tools::file_path_sans_ext(basename(p)))
  }) %>%
    dplyr::select(filename, co2_height, ch4_height, n2o_height)
  
  log_msg("  Peak heights table rows:", nrow(peak_heights))
  
  peak_heights <- peak_heights %>%
    mutate(filename = normalize_gc_filename(filename))
  
  # ---- diagnose filename matching ----
  missing_in_log  <- setdiff(unique(peak_heights$filename), unique(sample_log$filename))
  missing_in_peaks <- setdiff(unique(sample_log$filename), unique(peak_heights$filename))
  
  log_msg("INFO", "Filename match:",
          "overlap =", length(intersect(unique(peak_heights$filename), unique(sample_log$filename))),
          "| peak_only =", length(missing_in_log),
          "| log_only =", length(missing_in_peaks))
  
  if (length(missing_in_log) > 0) {
    log_msg("WARN", "Peak files missing in sample_log (showing 10):",
            paste(utils::head(missing_in_log, 10), collapse = ", "))
  }
  if (length(missing_in_peaks) > 0) {
    log_msg("WARN", "sample_log filenames missing among .txt (showing 10):",
            paste(utils::head(missing_in_peaks, 10), collapse = ", "))
  }
  
  log_msg("Key overlap:", length(intersect(unique(peak_heights$filename), unique(sample_log$filename))),
          "/", length(unique(peak_heights$filename)), "peak files")
  
  # ---- join with sample log ----
  gc_data_tidy <- dplyr::left_join(peak_heights, sample_log, by = "filename") %>%
    dplyr::mutate(
      row = dplyr::row_number(),
      chamber = dplyr::if_else(is.na(chamber), "STD", as.character(chamber))
    ) %>%
    dplyr::select(
      row,
      date_collected,
      site,
      ecosystem,
      chamber_no,
      timepoint,
      gc_run_date,
      filename,
      chamber,
      co2_height,
      ch4_height,
      n2o_height
    )
  
  meta_na <- gc_data_tidy %>%
    summarize(
      n = n(),
      na_date_collected = sum(is.na(date_collected)),
      na_site = sum(is.na(site)),
      na_ecosystem = sum(is.na(ecosystem)),
      na_timepoint = sum(is.na(timepoint)),
      na_gc_run_date = sum(is.na(gc_run_date))
    )
  
  log_msg("INFO", "Metadata NA counts:",
          "date_collected", meta_na$na_date_collected,
          "| site", meta_na$na_site,
          "| ecosystem", meta_na$na_ecosystem,
          "| timepoint", meta_na$na_timepoint,
          "| gc_run_date", meta_na$na_gc_run_date)
  
  # ---- output directory ----
  out_dir <- file.path(tidy_root, run_label)
  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    log_msg("  Created output directory:", out_dir)
  } else {
    log_msg("  Output directory already exists:", out_dir)
  }
  
  # ---- write peak heights ----
  peaks_outfile <- file.path(out_dir, paste0(run_label, "_gc_peak_heights.csv"))
  readr::write_csv(gc_data_tidy, peaks_outfile)
  log_msg("  ✓ Peak heights written:", peaks_outfile)
  
  # ---- write standards-only + qc ----
  gc_stds <- gc_data_tidy %>%
    dplyr::mutate(
      chamber_str = as.character(chamber),
      site_str    = as.character(site),
      is_std      = stringr::str_detect(chamber_str, stringr::regex("std", ignore_case = TRUE)) |
        stringr::str_detect(site_str,    stringr::regex("std", ignore_case = TRUE))
    ) %>%
    dplyr::filter(is_std)
  
  gc_stds_qc <- gc_stds %>%
    dplyr::mutate(
      qc_co2_low = co2_height < 900000,
      qc_ch4_low = ch4_height < 8750,
      qc_n2o_low = n2o_height < 12500,
      qc_any_low = qc_co2_low | qc_ch4_low | qc_n2o_low
    )
  
  stds_outfile <- file.path(out_dir, paste0(run_label, "_gc_standards.csv"))
  stds_qc_outfile <- file.path(out_dir, paste0(run_label, "_gc_standards_qc.csv"))
  
  readr::write_csv(gc_stds, stds_outfile)
  readr::write_csv(gc_stds_qc, stds_qc_outfile)
  
  log_msg("INFO", "Wrote:",
          basename(peaks_outfile),
          "|", basename(stds_outfile),
          "|", basename(stds_qc_outfile))
  
  log_msg("✔ Finished run (peak heights + standards):", run_dir)
  invisible(list(peaks_outfile = peaks_outfile, data = gc_data_tidy))
  
  gc_log_indent(-1)
  
  # Write per-run log next to outputs (optional but awesome)
  run_log_path <- file.path(out_dir, paste0(run_label, "_run.log"))
  write_run_log(run_log_path)
  
  log_run_footer(TRUE, run_label, "(log:", basename(run_log_path), ")")
  
}