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
#' @param .x A list or atomic vector.
#' @param .f A function, formula or atomic vector.
#' @param ... Other arguments supplied to \code{\link[purrr:map]{map}}
#' @return A list of the same length as \code{.x}. The list elemnts contain
#'   results and captured side effects as described in
#'   \code{\link[purrr:safely]{safely}} and
#'   \code{\link[purrr:quietly]{quietly}}.
#' @name collateral_mappers
#'
NULL

#' @rdname collateral_mappers
#' @importFrom purrr as_mapper safely map
#' @export
map_safely <- function(.x, .f, ...) {

  .f <- purrr::as_mapper(.f, ...)
  .f <- purrr::safely(.f)
  results <- purrr::map(.x, .f, ...)
  class(results) <- c('safely_mapped', class(results))
  results
}

#' @rdname collateral_mappers
#' @importFrom purrr as_mapper quietly map
#' @export
map_quietly <- function(.x, .f, ...) {

  .f <- purrr::as_mapper(.f, ...)
  .f <- purrr::quietly(.f)
  results <- purrr::map(.x, .f, ...)
  class(results) <- c('quietly_mapped', class(results))
  results
}
