# --- Step 6: convert per-chamber slope (amount/min) -> areal flux (amount/m2/s)

add_flux_units <- function(fits_qc,
                           chamber_vols,
                           co2_out_unit = "umol",   # CO2 slope is umol/min in your pipeline
                           ch4_out_unit = "nmol",   # CH4 slope is nmol/min
                           n2o_out_unit = "nmol",   # N2O slope is nmol/min
                           time_unit_in = "min",    # slope time base
                           time_unit_out = "s"      # desired time base
) {
  vols <- readr::read_csv(chamber_vols, show_col_types = FALSE) %>%
    mutate(
      ecosystem_name  = as.character(ecosystem_name),
      ecosystem_block = as.integer(ecosystem_block),
      rep             = as.integer(rep),
      radius_cm       = suppressWarnings(as.numeric(radius_cm)),
      diameter_cm     = suppressWarnings(as.numeric(diameter_cm))
    )
  
  # derive radius if missing but diameter exists
  vols <- vols %>%
    mutate(
      radius_cm = dplyr::case_when(
        !is.na(radius_cm) ~ radius_cm,
        is.na(radius_cm) & !is.na(diameter_cm) ~ diameter_cm / 2,
        TRUE ~ NA_real_
      ),
      area_m2 = pi * (radius_cm / 100)^2
    )
  
  if (any(is.na(vols$area_m2))) {
    nbad <- sum(is.na(vols$area_m2))
    stop("add_flux_units(): missing chamber area for ", nbad,
         " rows in chamber_vols (need radius_cm or diameter_cm).")
  }
  
  time_factor <- dplyr::case_when(
    time_unit_in == "min" & time_unit_out == "s"  ~ 1/60,
    time_unit_in == "s"   & time_unit_out == "min"~ 60,
    time_unit_in == time_unit_out                 ~ 1,
    TRUE ~ NA_real_
  )
  if (is.na(time_factor)) stop("add_flux_units(): unsupported time conversion.")
  
  out <- fits_qc %>%
    mutate(
      ecosystem_name  = as.character(ecosystem_name),
      ecosystem_block = as.integer(ecosystem_block),
      rep             = as.integer(rep),
      gas             = as.character(gas)
    ) %>%
    left_join(vols %>% select(ecosystem_name, ecosystem_block, rep, area_m2),
              by = c("ecosystem_name", "ecosystem_block", "rep"))
  
  if (any(is.na(out$area_m2))) {
    nbad <- sum(is.na(out$area_m2))
    stop("add_flux_units(): join to chamber_vols failed for ", nbad, " fit rows.")
  }
  
  # slope is amount per minute in your current fit
  # flux_per_s is amount / (m2 * s)
  out %>%
    mutate(
      slope_per_min = slope,
      flux_per_s = (slope_per_min / area_m2) * time_factor,
      flux_per_h = flux_per_s * 3600
    )
}