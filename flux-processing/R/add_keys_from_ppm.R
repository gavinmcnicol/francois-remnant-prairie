add_keys_from_ppm <- function(df) {
  out <- df %>%
    mutate(
      date_collected = suppressWarnings(lubridate::ymd(date_collected)),
      date = format(date_collected, "%y%m%d"),
      ecosystem = as.character(ecosystem),
      ecosystem_name  = stringr::str_extract(ecosystem, "^[A-Za-z]+"),
      ecosystem_block = suppressWarnings(as.integer(stringr::str_extract(ecosystem, "\\d+"))),
      rep = suppressWarnings(as.integer(as.character(chamber))),
      timepoint  = as.character(timepoint),
      time_point = suppressWarnings(as.integer(stringr::str_extract(timepoint, "\\d+")))
    )
  
  if (any(is.na(out$date))) {
    stop("add_keys_from_ppm(): NA date key after parsing date_collected")
  }
  
  out
}