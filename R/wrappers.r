# okay, time for another swing: instead of redefining safely() and quietly(), i'll
# make map() variants that wrap the passed function in safely() or quietly(). turns
# out it's the list column i need to class, not the elements, so this could work well.

#' @importFrom purrr as_mapper safely map
#' @export
map_safely <- function(.x, .f, ...) {

  .f <- purrr::as_mapper(.f, ...)
  .f <- purrr::safely(.f)
  results <- purrr::map(.x, .f, ...)
  class(results) <- c('safely_mapped', class(results))
  results
}

#' @importFrom purrr as_mapper safely map
#' @export
map_quietly <- function(.x, .f, ...) {

  .f <- purrr::as_mapper(.f, ...)
  .f <- quietly(.f)
  results <- purrr::map(.x, .f, ...)
  class(results) <- c('quietly_mapped', class(results))
  results
}
