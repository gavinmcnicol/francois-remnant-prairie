extract_gc_run_date_from_txt <- function(txt_path, n_lines = 200) {
  
  x <- readLines(txt_path, warn = FALSE, n = n_lines)
  
  get_key_value <- function(key) {
    i <- which(stringr::str_detect(
      x,
      paste0("^", stringr::fixed(key), "\\s+")
    ))
    if (!length(i)) return(NA_character_)
    
    line <- x[i[1]]
    
    parts <- strsplit(line, "\t", fixed = TRUE)[[1]]
    if (length(parts) < 2)
      parts <- strsplit(line, "\\s{2,}")[[1]]
    
    if (length(parts) < 2) return(NA_character_)
    stringr::str_trim(parts[2])
  }
  
  # ------------------------------------------------------------
  # 1) Prefer: Acquired (Sample Information)
  # ------------------------------------------------------------
  acquired_str <- get_key_value("Acquired")
  if (!is.na(acquired_str) && nzchar(acquired_str)) {
    dt <- suppressWarnings(lubridate::parse_date_time(
      acquired_str,
      orders = c(
        "mdy HMS p", "mdy HM p",
        "mdy HMS",   "mdy HM",
        "ymd HMS",   "ymd HM"
      )
    ))
    d <- suppressWarnings(as.Date(dt))
    if (!is.na(d)) return(d)
  }
  
  # ------------------------------------------------------------
  # 2) Fallback: Generated
  # ------------------------------------------------------------
  gen_str <- get_key_value("Generated")
  if (!is.na(gen_str) && nzchar(gen_str)) {
    dt <- suppressWarnings(lubridate::parse_date_time(
      gen_str,
      orders = c(
        "mdy HMS p", "mdy HM p",
        "mdy HMS",   "mdy HM",
        "ymd HMS",   "ymd HM"
      )
    ))
    d <- suppressWarnings(as.Date(dt))
    if (!is.na(d)) return(d)
  }
  
  # ------------------------------------------------------------
  # 3) Fallback: Output Date
  # ------------------------------------------------------------
  out_date_str <- get_key_value("Output Date")
  if (!is.na(out_date_str) && nzchar(out_date_str)) {
    d <- suppressWarnings(lubridate::mdy(out_date_str))
    if (!is.na(d)) return(d)
  }
  
  # ------------------------------------------------------------
  # 4) Give up
  # ------------------------------------------------------------
  as.Date(NA)
}