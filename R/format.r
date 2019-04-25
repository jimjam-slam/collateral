#' Additional functions required to integrate
#' \code{\link{map_safely}} and \code{\link{map_quietly}} with
#' the \code{pillar} package.
#' @keywords internal
#' @name collateral_extras
NULL

#' @rdname collateral_extras
#' @export
safely_mapped <- function(x) {
  as_safely_mapped(x)
}

#' @rdname collateral_extras
#' @export
quietly_mapped <- function(x) {
  as_quietly_mapped(x)
}

#' @rdname collateral_extras
#' @export
peacefully_mapped <- function(x) {
  as_peacefully_mapped(x)
}

#' @rdname collateral_extras
#' @export
as_safely_mapped <- function(x) {
  structure(x, class = "safely_mapped")
}

#' @rdname collateral_extras
#' @export
as_quietly_mapped <- function(x) {
  structure(x, class = "quietly_mapped")
}

#' @rdname collateral_extras
#' @export
as_peacefully_mapped <- function(x) {
  structure(x, class = "peacefully_mapped")
}

#' @rdname collateral_extras
#' @export
c.safely_mapped <- function(x, ...) {
  as_safely_mapped(NextMethod())
}

#' @rdname collateral_extras
#' @export
c.quietly_mapped <- function(x, ...) {
  as_quietly_mapped(NextMethod())
}

#' @rdname collateral_extras
#' @export
c.peacefully_mapped <- function(x, ...) {
  as_peacefully_mapped(NextMethod())
}

#' @rdname collateral_extras
#' @export
`[.safely_mapped` <- function(x, i) {
  as_safely_mapped(NextMethod())
}

#' @rdname collateral_extras
#' @export
`[.quietly_mapped` <- function(x, i) {
  as_quietly_mapped(NextMethod())
}

#' @rdname collateral_extras
#' @export
`[.peacefully_mapped` <- function(x, i) {
  as_peacefully_mapped(NextMethod())
}

# format functions actually parse the output and return styled strings --------

#' @rdname collateral_extras
#' @importFrom purrr is_empty
#' @importFrom crayon green red silver
#' @export
format.safely_mapped = function(x, ...) {
  # styled constants
  qu_R = crayon::green('R')
  qu_E = crayon::red('E')
  qu_none = crayon::silver('_')

  purrr::map_chr(x,
    ~ paste(
        if (is_empty(.$result))                     qu_none else qu_R,
        if (is_empty(.$error$message))              qu_none else qu_E,
        sep = ' '))
}

#' @rdname collateral_extras
#' @importFrom purrr is_empty map_chr
#' @importFrom crayon green make_style yellow white silver
#' @export
format.quietly_mapped = function(x, ...) {
  # styled constants
  qu_R = crayon::green('R')
  qu_W = crayon::make_style('orange')('W')
  qu_M = crayon::yellow('M')
  qu_O = crayon::white('O')
  qu_none = crayon::silver('_')

  purrr::map_chr(x,
    ~ paste(
      if (is_empty(.$result))                       qu_none else qu_R,
      if (is_empty(.$output) | all(.$output == '')) qu_none else qu_O,
      if (is_empty(.$message))                      qu_none else qu_M,
      if (is_empty(.$warning))                      qu_none else qu_W,
      sep = ' '))
}

#' @rdname collateral_extras
#' @importFrom purrr is_empty map_chr
#' @importFrom crayon green make_style red yellow white silver
#' @export
format.peacefully_mapped = function(x, ...) {
  # styled constants
  qu_R = crayon::green('R')
  qu_E = crayon::red('E')
  qu_W = crayon::make_style('orange')('W')
  qu_M = crayon::yellow('M')
  qu_O = crayon::white('O')
  qu_none = crayon::silver('_')

  purrr::map_chr(x,
    ~ paste(
      if (is_empty(.$result))                        qu_none else qu_R,
      if (is_empty(.$output) | all(.$output == ''))  qu_none else qu_O,
      if (is_empty(.$message))                       qu_none else qu_M,
      if (is_empty(.$warning))                       qu_none else qu_W,
      if (is_empty(.$error))                         qu_none else qu_E,
      sep = ' '))
}

# #' @importFrom knitr knit_print
# #' @importFrom purrr is_empty map_chr
# #' @export
# knit_print.safely_mapped = function(x, ...) {
#   # styled constants
#   qu_R = 'R'
#   qu_E = 'E'
#   qu_none = '_'
#
#   purrr::map_chr(x,
#     ~ paste(
#       if (is.null(.$result))                            qu_none else qu_R,
#       if (is.null(.$error) | is_empty(.$error$message)) qu_none else qu_E,
#       sep = ' '))
# }

#' @rdname collateral_extras
#' @export
print.safely_mapped = function(x, ...) {
  cat(format(x), sep = '\n')
  invisible(x)

}

#' @rdname collateral_extras
#' @export
print.quietly_mapped = function(x, ...) {
  cat(format(x), sep = '\n')
  invisible(x)
}

#' @rdname collateral_extras
#' @export
print.peacefully_mapped = function(x, ...) {
  cat(format(x), sep = '\n')
  invisible(x)
}
