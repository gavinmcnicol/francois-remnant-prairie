flag_bad_jumps_one_group <- function(df,
                                     value_col = "amount",
                                     k_jump = k_jump,
                                     k_out = k_out,
                                     use_mad = FALSE,
                                     max_level_drops = 2L,
                                     min_points_keep = 3L) {
  
  safe_z <- function(x, mu, sig) {
    if (!is.finite(mu) || !is.finite(sig)) return(NA_real_)
    if (sig <= 0) return(ifelse(isTRUE(all.equal(x, mu)), 0, Inf))
    abs(x - mu) / sig
  }
  
  df <- df %>%
    arrange(min_elapsed) %>%
    mutate(
      value = .data[[value_col]],
      delta = value - dplyr::lag(value)
    )
  
  center_scale_loo <- function(x, i) {
    xx <- x[-i]
    if (sum(!is.na(xx)) < 3) return(list(mu = NA_real_, sig = NA_real_))
    if (use_mad) {
      list(mu = stats::median(xx, na.rm = TRUE),
           sig = stats::mad(xx, na.rm = TRUE, constant = 1.4826))
    } else {
      list(mu = mean(xx, na.rm = TRUE),
           sig = stats::sd(xx, na.rm = TRUE))
    }
  }
  
  n <- nrow(df)
  df$drop_jump <- FALSE
  df$reason <- NA_character_
  
  # ------------------------------------------------------------
  # 1) Jump candidates: abs(delta_i) > k_jump * scale(deltas without i)
  # ------------------------------------------------------------
  deltas <- df$delta
  
  if (sum(!is.na(deltas)) >= 3) {
    
    delta_scale_loo <- vapply(seq_len(n), function(i) {
      if (is.na(deltas[i])) return(NA_real_)
      dd <- deltas[-i]
      if (sum(!is.na(dd)) < 2) return(NA_real_)
      if (use_mad) stats::mad(dd, na.rm = TRUE, constant = 1.4826) else stats::sd(dd, na.rm = TRUE)
    }, numeric(1))
    
    jump_idx <- which(!is.na(deltas) & !is.na(delta_scale_loo) &
                        abs(deltas) > k_jump * delta_scale_loo)
    
    for (i in jump_idx) {
      if (i < 2) next  # jump is between i-1 and i
      
      cs_i   <- center_scale_loo(df$value, i)
      cs_im1 <- center_scale_loo(df$value, i - 1)
      
      z_i   <- safe_z(df$value[i],   cs_i$mu,   cs_i$sig)
      z_im1 <- safe_z(df$value[i-1], cs_im1$mu, cs_im1$sig)
      
      # drop whichever point is more outlying, but only if it exceeds k_out
      if (max(z_i, z_im1, na.rm = TRUE) >= k_out) {
        idx_drop <- if (is.na(z_im1) || (!is.na(z_i) && z_i >= z_im1)) i else (i - 1)
        
        df$drop_jump[idx_drop] <- TRUE
        df$reason[idx_drop] <- paste0("JUMP->LEVEL_OUTLIER(z=", round(max(z_i, z_im1, na.rm = TRUE), 2), ")")
      }
    }
  }
  
  # ------------------------------------------------------------
  # 2) Level-only catch: drop up to max_level_drops extremes (can catch adjacent bad points)
  # never go below min_points_keep
  # ------------------------------------------------------------
  z_all <- vapply(seq_len(n), function(i) {
    cs <- center_scale_loo(df$value, i)
    safe_z(df$value[i], cs$mu, cs$sig)
  }, numeric(1))
  
  cand <- which(is.finite(z_all) & z_all >= k_out & !df$drop_jump)
  
  if (length(cand) > 0) {
    cand <- cand[order(z_all[cand], decreasing = TRUE)]
    
    max_can_drop <- max(0L, n - min_points_keep)
    take <- min(length(cand), max_level_drops, max_can_drop)
    
    if (take > 0) {
      idx_drop <- cand[seq_len(take)]
      df$drop_jump[idx_drop] <- TRUE
      df$reason[idx_drop] <- paste0("LEVEL_OUTLIER(z=", round(z_all[idx_drop], 2), ")")
    }
  }
  
  df
}