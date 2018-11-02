# format.X: for each collat_safe()- or collat_quiet()-wrapped output, return a character
# indicating with components are present

# TODO - do i need to define c.XXX and `[.XXX`?
# ref: https://cran.rstudio.com/web/packages/tibble/vignettes/extending.html

#' @export
safely_mapped <- function(x) {
  as_safely_mapped(x)
}

#' @export
quietly_mapped <- function(x) {
  as_quietly_mapped(x)
}

#' @export
as_safely_mapped <- function(x) {
  structure(x, class = "safely_mapped")
}

#' @export
as_quietly_mapped <- function(x) {
  structure(x, class = "quietly_mapped")
}

#' @export
c.safely_mapped <- function(x, ...) {
  as_safely_mapped(NextMethod())
}

#' @export
c.quietly_mapped <- function(x, ...) {
  as_quietly_mapped(NextMethod())
}

#' @export
`[.safely_mapped` <- function(x, i) {
  as_safely_mapped(NextMethod())
}

#' @export
`[.quietly_mapped` <- function(x, i) {
  as_quietly_mapped(NextMethod())
}

# format functions actually parse the output and return styled strings --------

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
        if (is.null(.$result))                            qu_none else qu_R,
        if (is.null(.$error) | is_empty(.$error$message)) qu_none else qu_E,
        sep = ' '))
}

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
      if (is.null(.$result))                            qu_none else qu_R,
      if (is.null(.$output)  | is_empty(.$output))      qu_none else qu_O,
      if (is.null(.$message) | is_empty(.$message))     qu_none else qu_M,
      if (is.null(.$warning) | is_empty(.$warning))     qu_none else qu_W,
      sep = ' '))
}

#' @export
print.safely_mapped = function(x, ...) {
  cat(format(x), sep = '\n')
  invisible(x)

}

#' @export
print.quietly_mapped = function(x, ...) {
  cat(format(x), sep = '\n')
  invisible(x)
}
