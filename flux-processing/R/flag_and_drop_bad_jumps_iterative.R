flag_and_drop_bad_jumps_iterative <- function(amount_df,
                                              n_passes = 2,
                                              k_jump = k_jump,
                                              k_out  = k_out,
                                              use_mad = FALSE) {
  
  if (n_passes < 1) stop("n_passes must be >= 1")
  
  df <- amount_df
  dropped_all <- tibble()
  
  for (pass in seq_len(n_passes)) {
    
    res <- flag_and_drop_bad_jumps(
      df,
      k_jump = k_jump,
      k_out  = k_out,
      use_mad = use_mad
    )
    
    n_drop <- nrow(res$dropped)
    log_msg("Jump/outlier filter pass", pass, "drops:", n_drop)
    
    # Stop early if nothing dropped this pass
    if (n_drop == 0) break
    
    dropped_all <- bind_rows(
      dropped_all,
      res$dropped %>% mutate(pass = pass)
    )
    
    # Continue with cleaned data
    df <- res$data
  }
  
  list(
    data    = df,
    dropped = dropped_all
  )
}