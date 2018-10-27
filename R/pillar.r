# report_*_color: add coloring to the report outputs.
# used by the pillar_shaft() functions

report_safely_color = function(x) {

  # colour present components
  x = sub('R', crayon::green('R'), x, fixed = TRUE)
  x = sub('E', crayon::red('E'), x, fixed = TRUE)

  # colour all absent components grey
  x = gsub('_', crayon::gray('_'), x, fixed = TRUE)
  format(x, justify = "left")
}

report_quietly_color = function(x) {

  # orange should fall back without 256 colours
  crayon_orange = crayon::make_style('orange')

  # colour present components
  x = sub('R', crayon::green('R'), x, fixed = TRUE)
  x = sub('W', crayon_orange('W'), x, fixed = TRUE)
  x = sub('M', crayon::yellow('M'), x, fixed = TRUE)
  x = sub('O', crayon::white('O'), x, fixed = TRUE)

  # colour all absent components grey
  x = gsub('_', crayon::gray('_'), x, fixed = TRUE)
  format(x, justify = "left")
}


#' @importFrom pillar pillar_shaft
#' @export
pillar_shaft.report_safely <- function(x, ...) {
  out <- format(x, formatter = report_safely_color)
  # out[is.na(x)] <- NA
  pillar::new_pillar_shaft_simple(out, align = "left",
    width = 3, min_width = 3, na_indent = 0)
}

#' @importFrom pillar pillar_shaft
#' @export
pillar_shaft.report_quietly <- function(x, ...) {
  out <- format(x, formatter = report_quietly_color)
  # out[is.na(x)] <- NA
  pillar::new_pillar_shaft_simple(out, align = "left",
    width = 7, min_width = 7, na_indent = 0)
}

