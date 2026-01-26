#------------------------------------------------------------
# Step 4c: Fit per chamber × gas
#------------------------------------------------------------
fit_flux_models <- function(amount_df_with_time) {
  
  long <- amount_df_with_time %>%
    select(date, ecosystem_name, ecosystem_block, rep, min_elapsed,
           co2_umol, ch4_nmol, n2o_nmol) %>%
    pivot_longer(
      cols = c(co2_umol, ch4_nmol, n2o_nmol),
      names_to = "gas",
      values_to = "amount"
    ) %>%
    mutate(
      gas = recode(gas,
                   co2_umol = "CO2_umol",
                   ch4_nmol = "CH4_nmol",
                   n2o_nmol = "N2O_nmol"),
      amount = as.numeric(amount),
      min_elapsed = as.numeric(min_elapsed)
    ) %>%
    # CRITICAL: only keep actual observed points for each gas
    filter(!is.na(amount), !is.na(min_elapsed))
  
  fits <- long %>%
    group_by(date, ecosystem_name, ecosystem_block, rep, gas) %>%
    group_modify(\(d, key) {
      
      n_used <- nrow(d)
      
      if (n_used < 3) {
        return(tibble(
          n = n_used,
          slope = NA_real_,
          slope_se = NA_real_,
          p_value = NA_real_,
          r.squared = NA_real_
        ))
      }
      
      m <- lm(amount ~ min_elapsed, data = d)
      tid <- broom::tidy(m)
      gl  <- broom::glance(m)
      slope_row <- tid %>% filter(term == "min_elapsed") %>% slice(1)
      
      tibble(
        n = n_used,
        slope = slope_row$estimate,
        slope_se = slope_row$std.error,
        p_value = slope_row$p.value,
        r.squared = gl$r.squared
      )
    }) %>%
    ungroup()
  
  fits
}