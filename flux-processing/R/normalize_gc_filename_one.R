normalize_gc_filename_one <- function(x) {
  x <- as.character(x)
  x <- stringr::str_trim(x)
  
  # strip extension if present
  x <- stringr::str_replace(x, "\\.txt$", "")
  x <- stringr::str_replace(x, "\\.gcd$", "")
  
  # common patterns:
  # "220812_006" -> keep
  # "220812-006" -> "220812_006"
  # "220812 006" -> "220812_006"
  x <- stringr::str_replace_all(x, "[-\\s]+", "_")
  
  # enforce zero-padding on trailing numeric chunk if present
  # e.g. "220812_6" -> "220812_006"
  m <- stringr::str_match(x, "^(\\d{6})_(\\d+)$")
  if (!is.na(m[1,1])) {
    run <- m[1,2]
    idx <- as.integer(m[1,3])
    if (!is.na(idx)) return(sprintf("%s_%03d", run, idx))
  }
  
  x
}

normalize_gc_filename <- function(x) vapply(x, normalize_gc_filename_one, character(1))