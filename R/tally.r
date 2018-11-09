#' Get counts of types of mapped side effects.
#'
#' Unlike \code{\link{summary}}, the tally functions return counts of individual
#' types of side effects. This makes them easy to use with
#' \code{\link[dplyr]{summarise}}.
#'
#' Importantly, the \code{tally} functions tell you how many elements returned a type
#' of side effect, \emph{not the number of those side effects}. Some list elements
#' might return more than one warning, for example, and these are not counted
#' separately.
#'
#' @param x A \code{safely_mapped} or \code{quietly_mapped} list to tally.
#' @return An integer vector of length 1.
#'
#' @examples
#'
#' library(magrittr)
#'
#' list("a", 10, 100) %>% map_safely(log) %>% tally_errors()
#' list(5, -12, 103) %>% map_quietly(log) %>% tally_warnings()
#'
#' suppressMessages(library(tidyverse))
#'
#' # if you're working with list-columns, the tally functions are useful
#' # in conjunction with dplyr::summarise()
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
#'   mutate(qlog = map_quietly(data, ~ log(.$wt))) %>%
#'   summarise(
#'     num_results = tally_results(qlog),
#'     num_warnings = tally_warnings(qlog))
#'
#' @name tally
NULL

#' @rdname tally
#' @importFrom purrr map_lgl
#' @export
tally_results = function(x) {
  if(!(is(x, 'safely_mapped') | is(x, 'quietly_mapped'))) {
    stop(paste('tally_results() is only usable on safely_mapped or',
      'quietly_mapped objects.'))
  }
  length(which(map_lgl(x, ~ !is.null(.$result))))
}

#' @rdname tally
#' @importFrom purrr map_lgl is_empty
#' @export
tally_errors = function(x) {
  if(!is(x, 'safely_mapped')) {
    stop('tally_errors() is only usable on safely_mapped objects.')
  }
  length(which(map_lgl(x,
    ~ !is.null(.$error) & !is_empty(.$error$message))))
}

#' @rdname tally
#' @importFrom purrr map_lgl is_empty
#' @export
tally_warnings = function(x) {
  if(!is(x, 'quietly_mapped')) {
    stop('tally_warnings() is only usable on safely_mapped objects.')
  }
  length(which(map_lgl(x,
    ~ !is.null(.$warning) & !is_empty(.$warning))))
}

#' @rdname tally
#' @importFrom purrr map_lgl is_empty
#' @export
tally_messages = function(x) {
  if(!is(x, 'quietly_mapped')) {
    stop('tally_messages() is only usable on safely_mapped objects.')
  }
  length(which(map_lgl(x,
      ~ !is.null(.$message) & !is_empty(.$message))))
}

#' @rdname tally
#' @importFrom purrr map_lgl is_empty
#' @export
tally_output = function(x) {
  if(!is(x, 'quietly_mapped')) {
    stop('tally_output() is only usable on safely_mapped objects.')
  }
  length(which(map_lgl(x,
      ~ !is.null(.$output) & !is_empty(.$output))))
}
