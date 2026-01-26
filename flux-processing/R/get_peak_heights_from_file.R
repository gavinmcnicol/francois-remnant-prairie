get_peak_heights_from_file <- function(path) {
  lines <- readr::read_lines(path)
  
  # Find a header line that has Peak and Height (tab or spaces)
  hdr_idx <- which(stringr::str_detect(lines, regex("peak", ignore_case = TRUE)) &
                     stringr::str_detect(lines, regex("height", ignore_case = TRUE)))
  
  if (length(hdr_idx) == 0) {
    return(tibble::tibble(
      file = basename(path),
      co2_height = NA_real_,
      ch4_height = NA_real_,
      n2o_height = NA_real_
    ))
  }
  
  hi <- hdr_idx[1]
  header <- lines[hi]
  
  # Split header on tabs OR 2+ spaces
  header_fields <- stringr::str_split(header, "\\t|\\s{2,}", simplify = TRUE)
  header_fields <- header_fields[header_fields != ""]
  header_fields <- stringr::str_trim(header_fields)
  
  height_col <- which(tolower(header_fields) == "height")
  name_col   <- which(tolower(header_fields) %in% c("name","compound","gas","component"))
  
  if (length(height_col) == 0) {
    return(tibble::tibble(file = basename(path), co2_height = NA_real_, ch4_height = NA_real_, n2o_height = NA_real_))
  }
  height_col <- height_col[1]
  
  # Read subsequent lines that look like data rows (start with a digit)
  data_lines <- lines[(hi + 1):length(lines)]
  data_lines <- data_lines[stringr::str_detect(data_lines, "^\\s*\\d+")]
  if (length(data_lines) == 0) {
    return(tibble::tibble(file = basename(path), co2_height = NA_real_, ch4_height = NA_real_, n2o_height = NA_real_))
  }
  
  parse_row <- function(s) {
    f <- stringr::str_split(s, "\\t|\\s{2,}", simplify = TRUE)
    f <- f[f != ""]
    f <- stringr::str_trim(f)
    f
  }
  
  rows <- lapply(data_lines, parse_row)
  
  # helper to get height by gas name if we have a name column
  get_by_name <- function(gas) {
    if (length(name_col) == 0) return(NA_real_)
    for (r in rows) {
      if (length(r) >= max(name_col, height_col) &&
          stringr::str_detect(r[name_col], regex(paste0("^", gas, "$"), ignore_case = TRUE))) {
        return(suppressWarnings(as.numeric(r[height_col])))
      }
    }
    NA_real_
  }
  
  co2 <- get_by_name("CO2")
  ch4 <- get_by_name("CH4")
  n2o <- get_by_name("N2O")
  
  # fallback: if names not found, assume first three peaks correspond to CO2, CH4, N2O (common in fixed methods)
  if (all(is.na(c(co2, ch4, n2o)))) {
    vals <- vapply(rows[1:min(3, length(rows))], \(r) {
      if (length(r) < height_col) return(NA_real_)
      suppressWarnings(as.numeric(r[height_col]))
    }, numeric(1))
    
    co2 <- vals[1]
    ch4 <- if (length(vals) >= 2) vals[2] else NA_real_
    n2o <- if (length(vals) >= 3) vals[3] else NA_real_
  }
  
  tibble::tibble(
    file = basename(path),
    co2_height = co2,
    ch4_height = ch4,
    n2o_height = n2o
  )
}