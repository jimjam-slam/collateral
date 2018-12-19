safe_mapper <- function(.f, .mapper, ...){
  .f <- purrr::as_mapper(.f, ...)
  .f <- purrr::safely(.f)
  results <- .mapper(.f, ...)
  class(results) <- c('safely_mapped', class(results))
  results
}

quiet_mapper <- function(.f, .mapper, ...) {
  .f <- purrr::as_mapper(.f, ...)
  .f <- purrr::quietly(.f)
  results <- .mapper(.f, ...)
  class(results) <- c('quietly_mapped', class(results))
  results
}

map2_wrapper <- function(.x, .y){
  function(.f, ...)
    purrr::map2(.x, .y, .f, ...)
}

map_wrapper <- function(.x){
  function(.f, ...)
    purrr::map(.x, .f, ...)
}

pmap_wrapper <- function(.l){
  function(.f, ...)
    purrr::pmap(.l, .f, ...)
}

#' Map safely or quietly over a list.
#'
#' `map_safely` and `map_quietly` are variants of
#' [purrr::map()] that:
#' 1. wrap the supplied function `.f` with either [purrr::safely()] or
#' [purrr::quietly()], and
#' 1. add a class to the returned output list in order to format it nicely when
#' it (or a tibble it appears in) is printed.
#'
#' `map_safely` will summarise the returned list with a fixed-width
#' string of two (spaced) columns:
#' 1. If a `result` component is present, `R` appears, and
#' 1. If an `error` component is present, `E` appears.
#'
#' If either component is missing, an underscore (`_`) appears in its
#' place.
#'
#' Similarly, `map_quietly` will summarise the returned list with a
#' fixed-width string of four (spaced) columns:
#' 1. If a `result` component is present, `R` appears,
#' 1. If an `output` component is present, `O` appears,
#' 1. If a `messages` component is present, `M` appears, and
#' 1. If a `warnings` component is present, `W` appears.
#'
#'  If any is missing, an underscore (`_`) appears in its
#'  place.
#'
#'  Variants for \href{https://purrr.tidyverse.org/reference/map2.html}{iterating over two or more inputs simultaneously}
#'  are also provided and function identically to their `purrr` counterparts:
#'  1. `map2_safely`
#'  1. `map2_quietly`
#'  1. `pmap_safely`
#'  1. `pmap_quietly`
#'
#'
#' @param .x A list or atomic vector.
#' @param .y A list or atomic vector, of the same length as `.x`.
#' @param .l A list of lists. The length of `.l` determines the number of
#'   arguments that `.f` will be called with. List names will be used if
#'   present.
#' @param .f A function, formula or atomic vector.
#' @param ... Other arguments supplied to [purrr::map()].
#' @return A list of the same length as `.x`. The list elements contain
#'   results and captured side effects as described in
#'   [purrr::safely()] and
#'   [purrr::quietly()].
#' @name collateral_mappers
#'
#' @examples
#'
#' library(magrittr)
#'
#' # like map(), these can be used to iterate over vectors or lists
#' list("a", 10, 100) %>% map_safely(log)
#' list(5, -12, 103) %>% map_quietly(log)
#'
#' suppressMessages(library(tidyverse))
#'
#' # if you're using tibbles, you can also iterate over list-columns,
#' # such as nested data frames
#' mtcars %>%
#'   rownames_to_column(var = "car") %>%
#'   as_data_frame() %>%
#'   select(car, cyl, disp, wt) %>%
#'   # spike some rows in cyl == 4 to make them fail
#'   mutate(wt = dplyr::case_when(
#'     wt < 2 ~ -wt,
#'     TRUE ~ wt)) %>%
#'   # nest and do some operations quietly()
#'   nest(-cyl) %>%
#'   mutate(qlog = map_quietly(data, ~ log(.$wt)))
#'
NULL

#' @rdname collateral_mappers
#' @importFrom purrr as_mapper safely map
#' @export
map_safely <- function(.x, .f, ...) {
  .map <- map_wrapper(.x)
  safe_mapper(.f, .map, ...)
}


#' @rdname collateral_mappers
#' @importFrom purrr as_mapper quietly map
#' @export
map_quietly <- function(.x, .f, ...) {
  .map <- map_wrapper(.x)
  quiet_mapper(.f, .map, ...)
}

#' @rdname collateral_mappers
#' @importFrom purrr as_mapper safely map2
#' @export
map2_safely <- function(.x, .y, .f, ...) {
  .map2 <- map2_wrapper(.x, .y)
  safe_mapper(.f, .map2, ...)
}


#' @rdname collateral_mappers
#' @importFrom purrr as_mapper quietly map2
#' @export
map2_quietly <- function(.x, .y, .f, ...) {
  .map2 <- map2_wrapper(.x, .y)
  quiet_mapper(.f, .map2, ...)
}

#' @rdname collateral_mappers
#' @importFrom purrr as_mapper safely pmap
#' @export
pmap_safely <- function(.l, .f, ...) {
  .pmap <- map_wrapper(.l)
  safe_mapper(.f, .pmap, ...)
}

#' @rdname collateral_mappers
#' @importFrom purrr as_mapper quietly pmap
#' @export
pmap_quietly <- function(.l, .f, ...) {
  .pmap <- map_wrapper(.l)
  quiet_mapper(.f, .pmap, ...)
}
