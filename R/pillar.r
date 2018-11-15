
#' @importFrom pillar pillar_shaft
#' @export
pillar_shaft.safely_mapped <- function(x, ...) {
  out <- format(x)
  pillar::new_pillar_shaft_simple(out, align = "left",
    width = 3, min_width = 3, na_indent = 0)
}

#' @importFrom pillar pillar_shaft
#' @export
pillar_shaft.quietly_mapped <- function(x, ...) {
  out <- format(x)
  pillar::new_pillar_shaft_simple(out, align = "left",
    width = 7, min_width = 7, na_indent = 0)
}

# -----------------------------------------------------------------------------

#' @importFrom pillar type_sum
#' @export
type_sum.safely_mapped <- function(x) { format(x) }

#' @importFrom pillar type_sum
#' @export
type_sum.quietly_mapped <- function(x) { format(x) }
