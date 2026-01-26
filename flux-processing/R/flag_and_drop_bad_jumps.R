flag_and_drop_bad_jumps <- function(amount_df,
                                    k_jump = k_jump,
                                    k_out = k_out,
                                    use_mad = FALSE) {
  
  # --- Validate required columns
  req_keys <- c("date", "ecosystem_name", "ecosystem_block", "rep", "time_point", "min_elapsed")
  missing_keys <- setdiff(req_keys, names(amount_df))
  if (length(missing_keys) > 0) {
    stop("flag_and_drop_bad_jumps(): missing required columns: ",
         paste(missing_keys, collapse = ", "))
  }
  
  gas_cols <- intersect(names(amount_df), c("co2_umol", "ch4_nmol", "n2o_nmol"))
  if (length(gas_cols) == 0) {
    stop("flag_and_drop_bad_jumps(): no gas columns found (expected co2_umol/ch4_nmol/n2o_nmol).")
  }
  
  # --- Pivot long to evaluate each gas uniformly
  long <- amount_df %>%
    mutate(
      date = as.character(date),
      ecosystem_name = as.character(ecosystem_name),
      ecosystem_block = as.integer(ecosystem_block),
      rep = as.integer(rep),
      time_point = as.integer(time_point),
      min_elapsed = as.numeric(min_elapsed)
    ) %>%
    pivot_longer(
      cols = all_of(gas_cols),
      names_to = "gas_col",
      values_to = "amount"
    ) %>%
    mutate(
      amount = as.numeric(amount),
      gas = recode(gas_col,
                   co2_umol = "CO2_umol",
                   ch4_nmol = "CH4_nmol",
                   n2o_nmol = "N2O_nmol")
    ) %>%
    select(-gas_col)
  
  # --- Apply your per-series rule
  long_flagged <- long %>%
    group_by(date, ecosystem_name, ecosystem_block, rep, gas) %>%
    group_modify(~ flag_bad_jumps_one_group(
      .x,
      value_col = "amount",
      k_jump = k_jump,
      k_out  = k_out,
      use_mad = use_mad
    )) %>%
    ungroup()
  
  # --- Audit table of drops
  dropped <- long_flagged %>%
    filter(isTRUE(drop_jump)) %>%
    select(date, ecosystem_name, ecosystem_block, rep,
           time_point, min_elapsed, gas, amount, delta, reason)
  
  # --- Keep the rest and return to wide
  kept <- long_flagged %>%
    filter(!isTRUE(drop_jump)) %>%
    select(-value, -delta, -drop_jump, -reason)
  
  wide_kept <- kept %>%
    pivot_wider(names_from = gas, values_from = amount)
  
  list(
    data = wide_kept,
    dropped = dropped
  )
}