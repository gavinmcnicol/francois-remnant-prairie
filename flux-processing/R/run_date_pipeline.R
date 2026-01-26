#------------------------------------------------------------
# Pipeline runner
#------------------------------------------------------------
run_date_pipeline <- function(
    tidy_root = "flux-processing/data/tidy",
    run_label = NULL,                 # e.g., "230718" or NULL for all
    overwrite = TRUE,
    min_r2 = 0.6,
    max_p  = 0.05,
    n_passes = 2,
    k_jump = 2,
    k_out  = 2,
    use_mad = FALSE
) {
  
  ppm_files <- list.files(
    tidy_root,
    pattern = "_ghg_ppm\\.csv$",
    full.names = TRUE,
    recursive = TRUE
  )
  
  if (!is.null(run_label)) {
    ppm_files <- ppm_files[stringr::str_detect(ppm_files, paste0("/", run_label, "/"))]
  }
  
  if (length(ppm_files) == 0) stop("No *_ghg_ppm.csv files found under ", tidy_root)
  
  chamber_vols <- file.path(tidy_root, "chamber_vols.csv")
  if (!file.exists(chamber_vols)) stop("Missing chamber_vols.csv at: ", chamber_vols)
  
  log_msg("Runs to process:", length(ppm_files))
  
  purrr::walk(ppm_files, \(pf) {
    
    rl <- stringr::str_extract(basename(pf), "^\\d{6}")
    out_dir <- dirname(pf)
    
    chamber_temps_file <- file.path(out_dir, "chamber_temps.csv")
    min_elapsed_file   <- file.path(out_dir, "min_elapsed.csv")
    
    if (!file.exists(chamber_temps_file)) stop("Missing chamber_temps.csv for run ", rl, " at: ", chamber_temps_file)
    if (!file.exists(min_elapsed_file))   stop("Missing min_elapsed.csv for run ", rl, " at: ", min_elapsed_file)
    
    amount_out <- file.path(out_dir, paste0(rl, "_ghg_amount.csv"))
    fits_out   <- file.path(out_dir, paste0(rl, "_flux_fits.csv"))
    qc_out     <- file.path(out_dir, paste0(rl, "_flux_fits_qc.csv"))
    jump_out   <- file.path(out_dir, paste0(rl, "_dropped_jumps.csv"))
    
    if (!overwrite && file.exists(qc_out)) {
      log_msg("SKIP (exists):", qc_out)
      return(invisible(NULL))
    }
    
    log_msg("→ Step 3–5 for run:", rl)
    
    ppm_df <- readr::read_csv(pf, show_col_types = FALSE)
    
    amount_df <- ppm_to_amount(
      ppm_df,
      chamber_vols  = chamber_vols,
      chamber_temps_file = chamber_temps_file
    )
    if (is.null(amount_df) || !is.data.frame(amount_df)) stop("ppm_to_amount() returned non-data-frame for run: ", rl)
    
    amount_df_time <- join_min_elapsed(amount_df, min_elapsed_file = min_elapsed_file)
    if (is.null(amount_df_time) || !is.data.frame(amount_df_time)) stop("join_min_elapsed() returned non-data-frame for run: ", rl)
    
    jump_res <- flag_and_drop_bad_jumps_iterative(
      amount_df_time,
      n_passes = n_passes,
      k_jump = k_jump,
      k_out  = k_out,
      use_mad = use_mad
    )
    
    amount_df_time_clean <- jump_res$data
    
    log_msg("Total drops across passes:", nrow(jump_res$dropped))
    if (nrow(jump_res$dropped) > 0) print(dplyr::count(jump_res$dropped, pass, gas))
    
    readr::write_csv(jump_res$dropped, jump_out)
    log_msg("✓ Wrote:", basename(jump_out), "| dropped rows:", nrow(jump_res$dropped))
    
    fits <- fit_flux_models(amount_df_time_clean)
   
    # Step 5: QC
    fits_qc <- qc_flux_fits(fits, min_r2 = min_r2, max_p = max_p)
    
    # Write outputs (cleaned series + fits + qc)
    readr::write_csv(amount_df_time_clean, amount_out)
    readr::write_csv(fits, fits_out)
    readr::write_csv(fits_qc, qc_out)
    
    # Step 6: slopes -> areal fluxes
    flux_out <- file.path(out_dir, paste0(rl, "_flux_fits_flux.csv"))
    log_msg("→ Step 6: writing flux fits to:", flux_out)
    
    fits_flux <- tryCatch(
      add_flux_units(
        fits_qc,
        chamber_vols = chamber_vols,
        time_unit_in = "min",
        time_unit_out = "s"
      ),
      error = function(e) {
        log_msg("ERROR in add_flux_units() for run", rl, ":", conditionMessage(e))
        return(NULL)
      }
    )
    
    if (is.null(fits_flux)) {
      log_msg("Step 6 skipped (fits_flux is NULL) for run:", rl)
    } else {
      readr::write_csv(fits_flux, flux_out)
      log_msg("✓ Wrote:", basename(flux_out), "| rows:", nrow(fits_flux))
    }
    
  })
  
  invisible(TRUE)
}