#' Determine how many elements contain a type of mapped side effect.
#'
#' Unlike [summary()], the tally functions return counts of individual
#' types of side effects. This makes them easy to use with
#' [dplyr::summarise()].
#'
#' Importantly, the tally functions tell you how many _elements_ returned a type
#' of side effect, not how many _side effects_ were returned. Some list elements
#' might return more than one warning, for example, and these are not counted
#' separately.
#'
#' @param x A ``safely_mapped` or `quietly_mapped` list to tally.
#' @return An integer vector of length 1.
#'
#' @examples
#'
#' library(tibble)
#' library(dplyr)
#' library(tidyr)
#' library(collateral)
#'
#' list("a", 10, 100) %>% map_safely(log) %>% tally_errors()
#' list(5, -12, 103) %>% map_quietly(log) %>% tally_warnings()
#'
#' # if you're working with list-columns, the tally functions are useful
#' # in conjunction with dplyr::summarise()
#' mtcars %>%
#'   rownames_to_column(var = "car") %>%
#'   as_tibble() %>%
#'   select(car, cyl, disp, wt) %>%
#'   # spike some rows in cyl == 4 to make them fail
#'   mutate(wt = dplyr::case_when(
#'     wt < 2 ~ -wt,
#'     TRUE ~ wt)) %>%
#'   # nest and do some operations quietly()
#'   nest(data = -cyl) %>%
#'   mutate(qlog = map_quietly(data, ~ log(.$wt))) %>%
#'   summarise(
#'     num_results = tally_results(qlog),
#'     num_warnings = tally_warnings(qlog))
#'
#' @name tally
NULL

#' @rdname tally
#' @export
tally_results = function(x) {
  length(which(has_results(x)))
}

#' @rdname tally
#' @export
tally_errors = function(x) {
  length(which(has_errors(x)))
}

#' @rdname tally
#' @export
tally_warnings = function(x) {
  length(which(has_warnings(x)))
}

#' @rdname tally
#' @export
tally_messages = function(x) {
  length(which(has_messages(x)))
}

#' @rdname tally
#' @export
tally_output = function(x) {
  length(which(has_output(x)))
}
