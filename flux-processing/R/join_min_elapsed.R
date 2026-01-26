#------------------------------------------------------------
# Step 4a: join elapsed minutes
#------------------------------------------------------------
join_min_elapsed <- function(amount_df, min_elapsed_file) {
  
  mins <- readr::read_csv(min_elapsed_file, show_col_types = FALSE) %>%
    mutate(
      date = as.character(date),
      ecosystem_name  = as.character(ecosystem_name),
      ecosystem_block = as.integer(ecosystem_block),
      rep = as.integer(rep),
      time_point = as.integer(time_point),
      min_elapsed = as.numeric(min_elapsed)
    ) %>%
    select(date, ecosystem_name, ecosystem_block, rep, time_point, min_elapsed)
  
  out <- amount_df %>%
    add_keys_from_ppm() %>%
    left_join(mins, by = c("date", "ecosystem_name", "ecosystem_block", "rep", "time_point"))
  
  if (any(is.na(out$min_elapsed))) {
    nbad <- sum(is.na(out$min_elapsed))
    ex <- out %>% filter(is.na(min_elapsed)) %>%
      select(date, ecosystem_name, ecosystem_block, rep, time_point, timepoint, chamber, date_collected) %>%
      head(10)
    stop("join_min_elapsed(): min_elapsed join failed for ", nbad, " rows. Examples:\n",
         paste(capture.output(print(ex)), collapse = "\n"))
  }
  
  out
}