---
output: github_document
---

# collateral <img src="man/figures/logo.svg" align="right" width="180px" />

Map complex operations `safely` or `quietly` (or both!), quickly see the captured side effectsand quickly spot and isolate captured side effects.

The `collateral` package extends the power of [`purrr`'s side effect-capturing functions](https://purrr.tidyverse.org/reference/safely.html), giving you:

* drop-in `map()` variants, allowing you to capture side effects from functions mapped over lists, vectors and list-columns;
* fancy tibble output, allowing you to see which rows delivered errors or side effects; and
* helpers for summarising side effects or filtering tibbles and lists for present side effects.

If you're not familiar with `purrr` or haven't used a list-column workflow in R before, the [`collateral` vignette](https://rensa.co/collateral/articles/collateral.html) shows you how it works, the benefits for your analysis and how `collateral` simplifies the process of handling complex mapped operations.

If you're already familiar with `purrr`, the [tl;dr](https://en.wikipedia.org/wiki/Wikipedia:Too_long;_didn%27t_read) is that [`collateral::map_safely()` and `collateral::map_quietly()` (and their `map2` and `pmap` variants)](https://rensa.co/collateral/reference/collateral_mappers.html) will automatically wrap your supplied function in `safely()` or `quietly()` and will provide enhanced `print()`ed output and tibble displays. You can then use the [`has_*()`](https://rensa.co/collateral/reference/has.html) and [`tally_*()`](https://rensa.co/collateral/reference/tally.html) functions to filter or summarise the returned tibbles or lists.

## Installation

You can install `collateral` on CRAN:

```r
install.packages('collateral')
```

Or install the development version using  [`devtools`](https://cran.r-project.org/web/packages/devtools/index.html):

```r
devtools::install_github('rensa/collateral')
```

## Example


```r
library(tidyverse)
```

```
## [37m-- [1mAttaching packages[22m --------------------------------------- tidyverse 1.2.1 --[39m
```

```
## [37m[32mv[37m [34mggplot2[37m 3.2.0     [32mv[37m [34mpurrr  [37m 0.3.2
## [32mv[37m [34mtibble [37m 2.1.3     [32mv[37m [34mdplyr  [37m 0.8.3
## [32mv[37m [34mtidyr  [37m 0.8.3     [32mv[37m [34mstringr[37m 1.4.0
## [32mv[37m [34mreadr  [37m 1.3.1     [32mv[37m [34mforcats[37m 0.4.0[39m
```

```
## [37m-- [1mConflicts[22m ------------------------------------------ tidyverse_conflicts() --
## [31mx[37m [34mdplyr[37m::[32mfilter()[37m masks [34mstats[37m::filter()
## [31mx[37m [34mdplyr[37m::[32mlag()[37m    masks [34mstats[37m::lag()[39m
```

```r
library(collateral)

test =
  # tidy up and trim down for the example
  mtcars %>%
  rownames_to_column(var = "car") %>%
  as_tibble() %>%
  select(car, cyl, disp, wt) %>%
  # spike some rows in cyl == 4 to make them fail
  mutate(wt = dplyr::case_when(
    wt < 2 ~ -wt,
    TRUE ~ wt)) %>%
  # nest and do some operations quietly()
  nest(-cyl) %>%
  mutate(qlog = map_quietly(data, ~ log(.$wt)))

test
```

```
## [38;5;246m# A tibble: 3 x 3[39m
##     cyl data              qlog    
##   [3m[38;5;246m<dbl>[39m[23m [3m[38;5;246m<list>[39m[23m            [3m[38;5;246m<collat>[39m[23m
## [38;5;250m1[39m     6 [38;5;246m<tibble [7 x 3]>[39m  [32mR[39m [90m_[39m [90m_[39m [90m_[39m 
## [38;5;250m2[39m     4 [38;5;246m<tibble [11 x 3]>[39m [32mR[39m [90m_[39m [90m_[39m [38;5;214mW[39m 
## [38;5;250m3[39m     8 [38;5;246m<tibble [14 x 3]>[39m [32mR[39m [90m_[39m [90m_[39m [90m_[39m
```

<!-- ![Example of styled `collateral` output](man/figures/collateral_example.png)

`collateral` uses `pillar` to style output, so supported terminals will also color the output! -->

## Support

If you have a problem with `collateral`, please don't hesitate to [file an issue](https://github.com/rensa/collateral/issues/new) or [contact me](twitter.com/rensa_co)!
