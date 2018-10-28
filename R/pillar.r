
#' @importFrom pillar pillar_shaft
#' @export
pillar_shaft.safely <- function(x, ...) {
  out <- format(x)
  # out[is.na(x)] <- NA
  pillar::new_pillar_shaft_simple(out, align = "left",
    width = 3, min_width = 3, na_indent = 0)
}

#' @importFrom pillar pillar_shaft
#' @export
pillar_shaft.quietly <- function(x, ...) {
  out <- format(x)
  # out[is.na(x)] <- NA
  pillar::new_pillar_shaft_simple(out, align = "left",
    width = 7, min_width = 7, na_indent = 0)
}

# -----------------------------------------------------------------------------
# get safely/quietly output to behave as an S3 vector element (even though it's
# a list) and to report correctly in tibbles:
# https://cran.rstudio.com/web/packages/tibble/vignettes/extending.html#fixing-list-columns

#' @importFrom pillar is_vector_s3
#' @export
is_vector_s3.safely <- function(x) TRUE

#' @importFrom pillar is_vector_s3
#' @export
is_vector_s3.quietly <- function(x) TRUE

#' @importFrom pillar obj_sum
#' @export
obj_sum.safely <- function(x) { rep("safely", length(x)) }

#' @importFrom pillar obj_sum
#' @export
obj_sum.quietly <- function(x) { rep("quietly", length(x)) }

#' @importFrom pillar type_sum
#' @export
type_sum.safely <- function(x) { "safely" }

#' @importFrom pillar type_sum
#' @export
type_sum.quietly <- function(x) { "quietly" }
