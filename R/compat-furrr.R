# Compatibility functions that allow to use purrr or furrr transparently

map <- function(.x, .f, ..., .parallel = FALSE) {
  if (.parallel) {
    return(furrr::future_map(.x, .f, ...))
  } else {
    return(purrr::map(.x, .f, ...))
  }
}

map2 <- function(.x, .y, .f, ..., .parallel = FALSE) {
  if (.parallel) {
    return(furrr::future_map2(.x, .y, .f, ...))
  } else {
    return(purrr::map2(.x, .y, .f, ...))
  }
}

pmap <- function(.l, .f, ..., .parallel = FALSE) {
  if (.parallel) {
    return(furrr::future_pmap(.l, .f, ...))
  } else {
    return(purrr::pmap(.l, .f, ...))
  }
}
