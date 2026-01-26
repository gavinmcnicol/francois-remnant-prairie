######
# ppm to mol of each GHG - required chamber_vols.csv chamber_temps.csv in same date dir.
######

ppm_to_mol <- function(ghg_ppm_file,
                       chamber_vols_file  = "chamber_vols.csv",
                       chamber_temps_file = "chamber_temps.csv",
                       pressure_atm = 1) {
  
  # constants
  R <- 8.205736e-5  # m3*atm/(mol*K)
  
  ppm <- readr::read_csv(ghg_ppm_file, show_col_types = FALSE) %>%
    add_keys_from_ppm()
  
  vols <- readr::read_csv(chamber_vols_file, show_col_types = FALSE) %>%
    mutate(
      ecosystem_name = as.character(ecosystem_name),
      ecosystem_block = as.integer(ecosystem_block),
      rep = as.integer(rep)
    ) %>%
    select(ecosystem_name, ecosystem_block, rep, total_vol_m3)
  
  temps <- readr::read_csv(chamber_temps_file, show_col_types = FALSE) %>%
    mutate(
      date = as.character(date),
      ecosystem_name = as.character(ecosystem_name),
      ecosystem_block = as.integer(ecosystem_block),
      rep = as.integer(rep),
      average_temp_kelvin = as.numeric(average_temp_kelvin)
    ) %>%
    select(date, ecosystem_name, ecosystem_block, rep, average_temp_kelvin)
  
  out <- ppm %>%
    left_join(vols,  by = c("ecosystem_name", "ecosystem_block", "rep")) %>%
    left_join(temps, by = c("date", "ecosystem_name", "ecosystem_block", "rep")) %>%
    mutate(
      # moles of air in the chamber
      n_air_mol = (pressure_atm * total_vol_m3) / (R * average_temp_kelvin),
      
      # convert ppm -> mol fraction -> mol gas
      co2_mol = n_air_mol * (co2_ppm * 1e-6),
      ch4_mol = n_air_mol * (ch4_ppm * 1e-6),
      n2o_mol = n_air_mol * (n2o_ppm * 1e-6),
      
      # preferred output units
      co2_umol = co2_mol * 1e6,
      ch4_nmol = ch4_mol * 1e9,
      n2o_nmol = n2o_mol * 1e9
    )
  
  # hard-fail if key joins didn’t work (prevents silent garbage)
  bad <- out %>%
    summarize(
      n_missing_vol = sum(is.na(total_vol_m3)),
      n_missing_tmp = sum(is.na(average_temp_kelvin)),
      n_missing_rep = sum(is.na(rep)),
      .groups = "drop"
    )
  if (bad$n_missing_vol > 0 || bad$n_missing_tmp > 0 || bad$n_missing_rep > 0) {
    stop("ppm_to_mol(): missing joins detected: ",
         "missing volume rows=", bad$n_missing_vol,
         " missing temp rows=", bad$n_missing_tmp,
         " missing rep=", bad$n_missing_rep)
  }
  
  out
}