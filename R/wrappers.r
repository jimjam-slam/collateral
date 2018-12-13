#' Map safely or quietly over a list.
#'
#' \code{map_safely} and \code{map_quietly} are variants of
#' \code{\link[purrr:map]{map}} that:
#' \enumerate{
#'   \item wrap the supplied function \code{.f} with either
#'     \code{\link[purrr:safely]{safely}} or
#'     \code{\link[purrr:quietly]{quietly}}, and
#'   \item add a class to the returned output list in order to format it
#'     nicely when it (or a tibble it appears in) is printed.
#'  }
#'
#'  \code{map_safely} will summarise the returned list with a fixed-width
#'  string of two (spaced) columns:
#'  \enumerate{
#'    \item If a \code{result} component is present, \code{R} appears, and
#'    \item If an \code{error} component is present, \code{E} appears.
#'  }
#'  If either component is missing, an underscore (\code{_}) appears in its
#'  place.
#'
#'  Similarly, \code{map_quietly} will summarise the returned list with a
#'  fixed-width string of four (spaced) columns:
#'  \enumerate{
#'    \item If a \code{result} component is present, \code{R} appears,
#'    \item If an \code{output} component is present, \code{O} appears,
#'    \item If a \code{messages} component is present, \code{M} appears, and
#'    \item If a \code{warnings} component is present, \code{W} appears.
#'  }
#'  If any is missing, an underscore (\code{_}) appears in its
#'  place.
#'
#'  Variants for \href{https://purrr.tidyverse.org/reference/map2.html}{iterating over two or more inputs simultaneously}
#'  are also provided and function identically to their \code{purrr} counterparts:
#'  \enumerate{
#'  \item \code{map2_safely}
#'  \item \code{map2_quietly}
#'  \item \code{pmap_safely}
#'  \item \code{pmap_quietly}
#'  }
#'
#'  Parallel variants based on \code{\href{https://davisvaughan.github.io/furrr/}{furrr}}
#'  are available:
#'  \enumerate{
#'  \item \code{future_map_safely}
#'  \item \code{future_map_quietly}
#'  \item \code{future_map2_safely}
#'  \item \code{future_map2_quietly}
#'  \item \code{future_pmap_safely}
#'  \item \code{future_pmap_quietly}
#'  }
#'
#' @param .x A list or atomic vector.
#' @param .y A list or atomic vector, of the same length as \code{.x}.
#' @param .l A list of lists. The length of \code{.l} determines the number of
#'   arguments that \code{.f} will be called with. List names will be used if
#'   present.
#' @param .f A function, formula or atomic vector.
#' @param ... Other arguments supplied to \code{\link[purrr:map]{map}}.
#' @return A list of the same length as \code{.x}. The list elements contain
#'   results and captured side effects as described in
#'   \code{\link[purrr:safely]{safely}} and
#'   \code{\link[purrr:quietly]{quietly}}.
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

  .f <- purrr::as_mapper(.f, ...)
  .f <- purrr::safely(.f)
  results <- map(.x, .f, ...)
  class(results) <- c('safely_mapped', class(results))
  results
}

#' @rdname collateral_mappers
#' @importFrom purrr as_mapper quietly
#' @export
map_quietly <- function(.x, .f, ...) {

  .f <- purrr::as_mapper(.f, ...)
  .f <- purrr::quietly(.f)
  results <- map(.x, .f, ...)
  class(results) <- c('quietly_mapped', class(results))
  results
}

#' @rdname collateral_mappers
#' @importFrom purrr as_mapper safely
#' @export
map2_safely <- function(.x, .y, .f, ...) {

  .f <- purrr::as_mapper(.f, ...)
  .f <- purrr::safely(.f)
  results <- map2(.x, .y, .f, ...)
  class(results) <- c('safely_mapped', class(results))
  results
}

#' @rdname collateral_mappers
#' @importFrom purrr as_mapper quietly
#' @export
map2_quietly <- function(.x, .y, .f, ...) {

  .f <- purrr::as_mapper(.f, ...)
  .f <- purrr::quietly(.f)
  results <- map2(.x, .y, .f, ...)
  class(results) <- c('quietly_mapped', class(results))
  results
}

#' @rdname collateral_mappers
#' @importFrom purrr as_mapper safely
#' @export
pmap_safely <- function(.l, .f, ...) {

  .f <- purrr::as_mapper(.f, ...)
  .f <- purrr::safely(.f)
  results <- pmap(.l, .f, ...)
  class(results) <- c('safely_mapped', class(results))
  results
}

#' @rdname collateral_mappers
#' @importFrom purrr as_mapper quietly
#' @export
pmap_quietly <- function(.l, .f, ...) {

  .f <- purrr::as_mapper(.f, ...)
  .f <- purrr::quietly(.f)
  results <- pmap(.l, .f, ...)
  class(results) <- c('quietly_mapped', class(results))
  results
}

#' @rdname collateral_mappers
#' @importFrom purrr partial
#' @export
future_map_safely <- purrr::partial(map_safely, .parallel = TRUE)

#' @rdname collateral_mappers
#' @importFrom purrr partial
#' @export
future_map_quietly <- purrr::partial(map_safely, .parallel = TRUE)

#' @rdname collateral_mappers
#' @importFrom purrr partial
#' @export
future_map2_safely <- purrr::partial(map2_safely, .parallel = TRUE)

#' @rdname collateral_mappers
#' @importFrom purrr partial
#' @export
future_map2_quietly <- purrr::partial(map2_safely, .parallel = TRUE)

#' @rdname collateral_mappers
#' @importFrom purrr partial
#' @export
future_pmap_safely <- purrr::partial(pmap_safely, .parallel = TRUE)

#' @rdname collateral_mappers
#' @importFrom purrr partial
#' @export
future_pmap_quietly <- purrr::partial(pmap_safely, .parallel = TRUE)
