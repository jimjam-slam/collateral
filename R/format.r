# format.X: for each safely()- or quietly()-wrapped output, return a character
# indicating with components are present

# TODO - do i need to define c.XXX and `[.XXX`?
# ref: https://cran.rstudio.com/web/packages/tibble/vignettes/extending.html

#' @export
as_safely <- function(x) {
  structure(x, class = "safely")
}

#' @export
as_quietly <- function(x) {
  structure(x, class = "quietly")
}

#' @export
c.safely <- function(x, ...) {
  as_safely(NextMethod())
}

#' @export
c.quietly <- function(x, ...) {
  as_quietly(NextMethod())
}

#' @export
`[.safely` <- function(x, i) {
  as_safely(NextMethod())
}

#' @export
`[.quietly` <- function(x, i) {
  as_quietly(NextMethod())
}

# format functions actually parse the output and return styled strings --------

#' @importFrom purrr is_empty
#' @export
format.safely = function(x, ...) {
  # styled constants
  qu_R = crayon::green('R')
  qu_E = crayon::red('E')
  qu_none = crayon::silver('_')

  format(
    paste(
      if (is.null(x$result))                            qu_none else qu_R,
      if (is.null(x$error) | is_empty(x$error$message)) qu_none else qu_E,
      sep = ' '),

    justify = "left")
}

#' @importFrom purrr is_empty
#' @export
format.quietly = function(x, ...) {
  # styled constants
  qu_R = crayon::green('R')
  qu_W = crayon::make_style('orange')('W')
  qu_M = crayon::yellow('M')
  qu_O = crayon::white('O')
  qu_none = crayon::silver('_')

  format(
    paste(
      if (is.null(x$result))                            qu_none else qu_R,
      if (is.null(x$output)  | is_empty(x$output))      qu_none else qu_O,
      if (is.null(x$message) | is_empty(x$message))     qu_none else qu_M,
      if (is.null(x$warning) | is_empty(x$warning))     qu_none else qu_W,
      sep = ' '),
    justify = "left")
}

#' @export
print.safely = function(x, ...) {
  cat(format(x), sep = '\n')
  invisible(x)
}

#' @export
print.quietly = function(x, ...) {
  cat(format(x), sep = '\n')
  invisible(x)
}
