#' Summarise mapped side effects.
#'
#' The \code{summary} method for a \code{safely_mapped} or \code{quietly_mapped}
#' list (or list-column) prints out the total number of elements (rows), as well
#' as the number that each returned results and errors (for
#' \code{safely_mapped}) or returned results, output, messages and warnings (for
#' \code{quietly_mapped}). It also invisibly returns a named vector with these
#' counts.
#'
#' Although the output can be used in tidy workflows (for automated testing, for
#' example), the tally functions (such as \code{\link{tally_results}}) tend to
#' be more convenient for this purpose.
#'
#' @param x A \code{safely_mapped} or \code{quietly_mapped} list to summarise.
#' @return A named vector containing counts of the components named in
#'   \code{\link{map_safely}}.
#'
#' @examples
#'
#' library(magrittr)
#'
#' list("a", 10, 100) %>% map_safely(log) %>% summary()
#' list(5, -12, 103) %>% map_quietly(log) %>% summary()
#'
#' @name summary
NULL


#' @rdname summary
#' @importFrom purrr is_empty map_lgl
#' @importFrom crayon green red
#' @export
summary.safely_mapped = function(x, ...) {
  counts = c(
    result = length(which(map_lgl(x, ~ !is.null(.$result)))),
    error = length(which(map_lgl(x,
      ~ !is.null(.$error) & !is_empty(.$error$message)))))

  cat(
    paste(length(x), 'elements in total.\n'),
    crayon::green(paste(counts['result'], 'elements returned results, and\n')),
    crayon::red(paste(counts['error'], 'elements encountered errors.\n')),
    sep = '')

  invisible(counts)
}

#' @rdname summary
#' @importFrom purrr is_empty map_lgl
#' @importFrom crayon green white yellow make_style
#' @export
summary.quietly_mapped = function(x, ...) {

  counts = c(
    result = length(which(map_lgl(x, ~ !is.null(.$result)))),
    output = length(which(map_lgl(x,
      ~ !is.null(.$output) & !is_empty(.$output)))),
    message = length(which(map_lgl(x,
      ~ !is.null(.$message) & !is_empty(.$message)))),
    warning = length(which(map_lgl(x,
      ~ !is.null(.$warning) & !is_empty(.$warning)))))

  cat(
    paste(length(x), 'elements in total.\n'),
    crayon::green(paste(counts['result'], 'elements returned results,\n')),
    crayon::white(paste(counts['output'], 'elements delivered output,\n')),
    crayon::yellow(paste(
      counts['message'], 'elements delivered messages, and\n')),
    crayon::make_style('orange')(paste(
      counts['warning'], 'elements delivered warnings.\n')),
    sep = '')

  invisible(counts)
}
