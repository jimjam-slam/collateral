
<!-- README.md is generated from README.Rmd. Please edit that file -->

# collateral <img src="man/figures/logo.svg" align="right" width="180px" style="padding-left: 1rem;" />

<!-- badges: start -->

![packageversion](https://img.shields.io/badge/Package%20version-0.5.2-orange.svg?style=flat-square)\]
![Last-changedate](https://img.shields.io/badge/last%20change-2021--09--20-yellowgreen.svg)
[![lifecycle](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable-1)
[![CRAN
status](https://www.r-pkg.org/badges/version/collateral)](https://cran.r-project.org/package=collateral)
[![license](https://img.shields.io/github/license/mashape/apistatus.svg)](https://choosealicense.com/licenses/mit/)

<!-- badges: end -->

R is great at automating a data analysis over many groups in a dataset,
but errors, warnings and other side effects can stop it in its tracks.
There’s nothing worse than returning after an hour (or a day!) and
discovering that 90% of your data wasn’t analysed because one group
threw an error.

With `collateral`, you can capture side effects like errors to prevent
them from stopping execution. Once your analysis is done, you can see a
tidy view of the groups that failed, the ones that returned a results,
and the ones that threw warnings, messages or other output as they ran.

You can even filter or summarise a data frame based on these side
effects to automatically continue analysis without failed groups (or
perhaps to show a report of the failed ones).

For example, this screenshot shows a data frame that’s been nested using
groups of the `cyl` column. The `qlog` column shows the results of an
operation that’s returned results for all three groups but also printed
a warning for one of them:

![Screenshot of a nested dataframe, including a column that shows the
results of an operation quietly mapped with
collateral](man/figures/collateral_example.png)

If you’re not familiar with `purrr` or haven’t used a list-column
workflow in R before, the [`collateral`
vignette](https://collateral.jamesgoldie.dev/articles/collateral.html)
shows you how it works, the benefits for your analysis and how
`collateral` simplifies the process of handling complex mapped
operations.

If you’re already familiar with `purrr`, the
[tl;dr](https://en.wikipedia.org/wiki/Wikipedia:Too_long;_didn%27t_read)
is that [`collateral::map_safely()` and `collateral::map_quietly()` (and
their `map2` and `pmap`
variants)](https://collateral.jamesgoldie.dev/reference/collateral_mappers.html)
will automatically wrap your supplied function in `safely()` or
`quietly()` (or both: `peacefully()`) and will provide enhanced printed
output and tibble displays. You can then use the
[`has_*()`](https://collateral.jamesgoldie.dev/reference/has.html) and
[`tally_*()`](https://collateral.jamesgoldie.dev/reference/tally.html)
functions to filter or summarise the returned tibbles or lists.

## Installation

You can install collateral in several ways:

1. The release version of collateral is available [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("collateral")
```

2. The development version is available on my R-Universe with:

``` r
install.packages("collateral", repos = "https://jimjam-slam.r-universe.dev")
```

3. Or you can get the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("jimjam-slam/collateral")
```

## Example

This example uses the famous `mtcars` dataset—but first, we’re going to
sabotage a few of the rows by making them negative. The `log` function
produces `NaN` with a warning when you give it a negative number.

It’d be easy to miss this in a non-interactive script if you didn’t
explicitly test for the presence of `NaN`! Thankfully, with collateral,
you can see which operations threw errors, which threw warnings, and
which produced output:

``` r
library(tibble)
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
library(tidyr)
library(collateral)

test <-
  # tidy up and trim down for the example
  mtcars %>%
  rownames_to_column(var = "car") %>%
  as_tibble() %>%
  select(car, cyl, disp, wt) %>%
  # spike some rows in cyl == 4 to make them fail
  mutate(wt = dplyr::case_when(
    wt < 2 ~ -wt,
    TRUE ~ wt)) %>%
  # nest and do some operations peacefully
  nest(data = -cyl) %>%
  mutate(qlog = map_peacefully(data, ~ log(.$wt)))

# look at the results
test
#> # A tibble: 3 × 3
#>     cyl data              qlog     
#>   <dbl> <list>            <collat> 
#> 1     6 <tibble [7 × 3]>  R _ _ _ _
#> 2     4 <tibble [11 × 3]> R _ _ W _
#> 3     8 <tibble [14 × 3]> R _ _ _ _
```

Here, we can see that all operations produced output (because `NaN` is
still output)—but a few of them also produced warnings! You can then see
those warnings…

``` r
test %>% mutate(qlog_warning = map_chr(qlog, "warnings", .null = NA))
#> # A tibble: 3 × 4
#>     cyl data              qlog      qlog_warning 
#>   <dbl> <list>            <collat>  <chr>        
#> 1     6 <tibble [7 × 3]>  R _ _ _ _ <NA>         
#> 2     4 <tibble [11 × 3]> R _ _ W _ NaNs produced
#> 3     8 <tibble [14 × 3]> R _ _ _ _ <NA>
```

… filter on them…

``` r
test %>% filter(!has_warnings(qlog))
#> # A tibble: 2 × 3
#>     cyl data              qlog     
#>   <dbl> <list>            <collat> 
#> 1     6 <tibble [7 × 3]>  R _ _ _ _
#> 2     8 <tibble [14 × 3]> R _ _ _ _
```

… or get a summary of them, for either interactive or non-interactive
purposes:

``` r
summary(test$qlog)
#> 3 elements in total.
#> 3 elements returned results,
#> 3 elements delivered output,
#> 0 elements delivered messages,
#> 1 element delivered warnings, and
#> 0 elements threw an error.
```

## Other features

The collateral package is now fully integrated with the `furrr` package,
so you can safely and quietly iterate operations across CPUs cores or
remote nodes. All collateral mappers have `future_*`-prefixed variants
for this purpose.

## Support

If you have a problem with `collateral`, please don’t hesitate to [file
an issue](https://github.com/jimjam-slam/collateral/issues/new) or
[contact me](https://twitter.com/jimjam_slam/)!
