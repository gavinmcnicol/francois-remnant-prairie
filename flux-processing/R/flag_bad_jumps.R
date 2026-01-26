flag_bad_jumps <- function(df, gas_col, k = 3) {
  
  df <- df %>%
    arrange(min_elapsed) %>%
    mutate(
      delta = .data[[gas_col]] - lag(.data[[gas_col]])
    )
  
  if (sum(!is.na(df$delta)) < 3) {
    df$drop_jump <- FALSE
    return(df)
  }
  
  df <- df %>%
    rowwise() %>%
    mutate(
      sd_others = sd(delta[-cur_group_rows()], na.rm = TRUE),
      drop_jump = !is.na(delta) & abs(delta) > k * sd_others
    ) %>%
    ungroup()
  
  df
}








