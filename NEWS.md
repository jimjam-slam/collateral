# collateral 0.5.1

## Minor changes

* Minor changes to the vignette and documentation
* Changed the use of `purrr::nest` in examples to maintain compatibility with updates to purrr
* Added the `list` class to returned output to ensure compatibility with tibble 3.0
* Fixed bug in `pmap_*()` functions ([issue #11](https://github.com/rensa/collateral/issues/11))

# collateral 0.5.0

## Major changes 

* Added `map_peacefully` variants that combine `map_safely` and `map_quietly`
* Added `future_` variants that integrate with the [`furrr`](https://cran.r-project.org/package=furrr) package to allow parallel processing

## Minor changes

* Some internal rejiggering
* Custom pkgdown site theme!
* Vignette improvements


# collateral 0.4.2

* First CRAN release!

# collateral 0.4.1

* Added a `NEWS.md` file to track changes to the package.

# collateral 0.4

* Added `has_*()` functions for use with `dplyr:filter()`.
* Altered the `tally_*()` functions to use `has_*()` and the `summary()` methods to use `tally_*()`.

# collateral 0.3

* Added `tally_*()` functions for use with `dplyr::summarise()`.

# collateral 0.2

* Added `summary()` methods to report the count of types of side effects in a column.
* Added a pkgdown site
* Add a sick hex sticker!

# collateral 0.1

* Improved documentation
* Lowered R version dependency
