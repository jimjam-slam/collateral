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
#' example), tally functions like \code{\link{tally_results}} tend to be more
#' convenient for this purpose.
#'
#' Importantly, the \code{summary} functions tell you how many elements
#' # returned a type of side effect, \emph{not the number of those side
#' effects}. Some list elements might return more than one warning, for
#' example, and these are not counted separately.
#'
#' @param object A \code{safely_mapped} or \code{quietly_mapped} list to summarise.
#' @param ... Other arguments passed to \code{summary}.
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
summary.safely_mapped = function(object, ...) {

  counts = c(result = tally_results(object), error = tally_errors(object))

  cat(
    paste0(
      length(object),
      ' element',
      if (length(object) == 1) '' else 's',
      ' in total.\n'),
    crayon::green(paste0(
      counts['result'],
      ' element',
      if (counts['result'] == 1) '' else 's',
      ' returned results, and\n')),
    crayon::red(paste0(
      counts['error'],
      ' element',
      if (counts['error'] == 1) '' else 's',
      ' encountered errors.\n')),
    sep = '')

  invisible(counts)
}

#' @rdname summary
#' @importFrom purrr is_empty map_lgl
#' @importFrom crayon green white yellow make_style
#' @export
summary.quietly_mapped = function(object, ...) {

  counts = c(result = tally_results(object), output = tally_output(object),
    message = tally_messages(object), warning = tally_warnings(object))

  cat(
    paste0(
      length(object),
      ' element',
      if (length(object) == 1) '' else 's',
      ' in total.\n'),
    crayon::green(paste(
      counts['result'],
      ' element',
      if (counts['result'] == 1) '' else 's',
      ' returned results,\n')),
    crayon::white(paste(
      counts['output'],
      ' element',
      if (counts['output'] == 1) '' else 's',
      ' delivered output,\n')),
    crayon::yellow(paste(
      counts['message'],
      ' element',
      if (counts['message'] == 1) '' else 's',
      ' delivered messages, and\n')),
    crayon::make_style('orange')(paste(
      counts['warning'],
      ' element',
      if (counts['warning'] == 1) '' else 's',
      ' delivered warnings.\n')),
    sep = '')

  invisible(counts)
}
