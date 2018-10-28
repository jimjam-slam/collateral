# because purrr::safely() and purrr::quietly() are closures that return
# capture_error() and capture_output(), i've redefined them to instead return
# wrapper fns of mins that addsimply  an s3 class to the output of the purrr
# equivalents.
# (these are in purrr/R/output.R)

#' @export
safely = function(.f, otherwise = NULL, quiet = TRUE) {
  .f = as_mapper(.f)
  function(...) capture_error_wrapper(.f(...), otherwise, quiet)
}

#' @export
quietly = function(.f) {
  .f = as_mapper(.f)
  function(...) capture_output_wrapper(.f(...))
}

# NB - capture_*() are *internal* purrr functions. this package can't
# go to cran as long as i use ::: to access them this way!

#' @importFrom purrr capture_error
capture_error_wrapper = function(...) {
 result = purrr:::capture_error(...)
 class(result) = c(class(result), 'safely')
 result
}

#' @importFrom purrr capture_output
capture_output_wrapper = function(...) {
 result = purrr:::capture_output(...)
 class(result) = c(class(result), 'quietly')
 result
}
