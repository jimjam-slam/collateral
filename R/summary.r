#' Summarise mapped side effects.
#'
#' The `summary()` method for a `safely_mapped` or `quietly_mapped`
#' list (or list-column) prints out the total number of elements (rows), as well
#' as the number that each returned results and errors (for
#' `safely_mapped`) or returned results, output, messages and warnings (for
#' `quietly_mapped`). It also invisibly returns a named vector with these
#' counts.
#'
#' Although the output can be used in tidy workflows (for automated testing, for
#' example), tally functions like [tally_results()] tend to be more
#' convenient for this purpose.
#'
#' Importantly, the `summary()` method tells you how many elements were
#' returned a type of side effect, _not the number of those side
#' effects_. Some list elements might return more than one warning, for
#' example, and these are not counted separately.
#'
#' @param object A `safely_mapped` or `quietly_mapped` list to summarise.
#' @param ... Other arguments passed to `summary()`.
#' @return A named vector containing counts of the components named in
#'   [map_safely()].
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
    paste(
      length(object),
      if (length(object) == 1) 'element' else 'elements',
      'in total.\n'),
    crayon::green(paste(
      counts['result'],
      if (counts['result'] == 1) 'element' else 'elements',
      'returned results, and\n')),
    crayon::red(paste(
      counts['error'],
      if (counts['error'] == 1) 'element' else 'elements',
      'encountered errors.\n')),
    sep = '')

  invisible(counts)
}

#' @rdname summary
#' @importFrom purrr is_empty map_lgl
#' @importFrom crayon green white yellow make_style
#' @export
summary.quietly_mapped = function(object, ...) {

  counts = c(
    result = tally_results(object), output = tally_output(object),
    message = tally_messages(object), warning = tally_warnings(object))

  cat(
    paste(
      length(object),
      if (length(object) == 1) 'element' else 'elements',
      'in total.\n'),
    crayon::green(paste(
      counts['result'],
      if (counts['result'] == 1) 'element' else 'elements',
      'returned results,\n')),
    crayon::white(paste(
      counts['output'],
      if (counts['output'] == 1) 'element' else 'elements',
      'delivered output,\n')),
    crayon::yellow(paste(
      counts['message'],
      if (counts['message'] == 1) 'element' else 'elements',
      'delivered messages, and\n')),
    crayon::make_style('orange')(paste(
      counts['warning'],
      if (counts['warning'] == 1) 'element' else 'elements',
      'delivered warnings.\n')),
    sep = '')

  invisible(counts)
}

#' @rdname summary
#' @importFrom purrr is_empty map_lgl
#' @importFrom crayon green white yellow red make_style
#' @export
summary.peacefully_mapped = function(object, ...) {

  counts = c(
    result = tally_results(object), output = tally_output(object),
    message = tally_messages(object), warning = tally_warnings(object),
    error = tally_errors(object))

  cat(
    paste(
      length(object),
      if (length(object) == 1) 'element' else 'elements',
      'in total.\n'),
    crayon::green(paste(
      counts['result'],
      if (counts['result'] == 1) 'element' else 'elements',
      'returned results,\n')),
    crayon::white(paste(
      counts['output'],
      if (counts['output'] == 1) 'element' else 'elements',
      'delivered output,\n')),
    crayon::yellow(paste(
      counts['message'],
      if (counts['message'] == 1) 'element' else 'elements',
      'delivered messages,\n')),
    crayon::make_style('orange')(paste(
      counts['warning'],
      if (counts['warning'] == 1) 'element' else 'elements',
      'delivered warnings, and\n')),
    crayon::red(paste(
      counts['error'],
      if (counts['error'] == 1) 'element' else 'elements',
      'threw an error.')),
    sep = '')

  invisible(counts)
}
