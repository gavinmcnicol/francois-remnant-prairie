library(tidyverse)
library(lubridate)

# --- constants ---
R_gas <- 8.314462618  # J mol^-1 K^-1
P_pa  <- 101325       # assume 1 atm; adjust if you have site pressure

add_chamber_amounts <- function(ghg_ppm_file, vols = chamber_vols, temps = chamber_temps) {
  
  dir      <- dirname(ghg_ppm_file)
  base_run <- sub("_ghg_ppm\\.csv$", "", basename(ghg_ppm_file))  # e.g., "230802"
  
  dat <- readr::read_csv(ghg_ppm_file, show_col_types = FALSE) %>%
    mutate(
      # derive join keys to match your vols/temps tables
      ecosystem_name  = stringr::str_extract(ecosystem, "^[A-Za-z]+"),
      ecosystem_block = suppressWarnings(as.integer(stringr::str_extract(ecosystem, "\\d+"))),
      rep             = suppressWarnings(as.integer(chamber_no)),
      chamber         = paste0(ecosystem_name, " ", ecosystem_block, "-", rep),
      
      run_label = base_run,
      date_key  = base_run
    )
  
  dat2 <- dat %>%
    left_join(vols,  by = c("ecosystem_name","ecosystem_block","rep","chamber")) %>%
    left_join(temps, by = c("date_key" = "date", "ecosystem_name","ecosystem_block","rep","chamber")) %>%
    mutate(
      # sanity checks
      total_vol_m3 = as.numeric(total_vol_m3),
      average_temp_kelvin = as.numeric(average_temp_kelvin),
      
      n_air_mol = (P_pa * total_vol_m3) / (R_gas * average_temp_kelvin),
      
      # ppm -> amount in chamber
      co2_umol = n_air_mol * co2_ppm,          # (mol * ppm * 1e-6) * 1e6 = mol*ppm
      ch4_nmol = n_air_mol * ch4_ppm * 1e3,    # (mol * ppm * 1e-6) * 1e9 = mol*ppm*1e3
      n2o_nmol = n_air_mol * n2o_ppm * 1e3
    )
  
  out_file <- file.path(dir, paste0(base_run, "_ghg_amounts.csv"))
  readr::write_csv(dat2, out_file)
  
  message("Wrote: ", out_file)
  invisible(dat2)
}