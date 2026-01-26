#------------------------------------------------------------
# Step 5: QC flags
#------------------------------------------------------------
qc_flux_fits <- function(fits, min_r2 = 0.6, max_p = 0.05) {
  fits %>%
    mutate(
      qc_flag = case_when(
        is.na(slope) ~ "MISSING",
        n < 3 ~ "TOO_FEW_POINTS",
        is.na(r.squared) ~ "MISSING",
        r.squared < min_r2 ~ "LOW_R2",
        p_value > max_p ~ "NON_SIG",
        TRUE ~ "OK"
      )
    )
}