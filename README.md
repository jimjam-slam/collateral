# collateral

Automatically map complex operations `safely` or `quietly`, and quickly see the captured side effects.

The `collateral` package extends the power of [`purrr::safely()`](https://purrr.tidyverse.org/reference/safely.html) and `purrr::quietly()`, providing variants of [`map()`](https://purrr.tidyverse.org/reference/map.html)—`map_safely()` and `map_quietly()`—that automatically apply the appropriate function wrapper _and_ print the resulting output nicely.

This is especially useful for tidy workflows: you can [`nest()`](https://tidyr.tidyverse.org/reference/nest.html) a data frame, apply a complex operation (such as building a regression model or building and saving a plot) to each group and quickly see which groups need to be inspected in more detail:

## Installation

We'll submit to CRAN soon, but for now, use [`devtools`](https://cran.r-project.org/web/packages/devtools/index.html) to install `collateral`:

```r
devtools::install_github('rensa/collateral')
```

## Example

```r
library(tidyverse)
library(collateral)

test =
  # tidy up and trim down for the example
  mtcars %>%
  rownames_to_column(var = "car") %>%
  as_data_frame() %>%
  select(car, cyl, disp, wt) %>%
  # spike some rows in cyl == 4 to make them fail
  mutate(wt = dplyr::case_when(
    wt < 2 ~ -wt,
    TRUE ~ wt)) %>%
  # nest and do some operations quietly()
  nest(-cyl) %>%
  mutate(qlog = map_quietly(data, ~ log(.$wt)))

test
#> # A tibble: 3 x 4
#>     cyl data              qlog
#>   <dbl> <list>            <collat>
#> 1     6 <tibble [7 x 3]>  R O _ _
#> 2     4 <tibble [11 x 3]> R O _ W
#> 3     8 <tibble [14 x 3]> R O _ _
```

![Example of styled `collateral` output](blob/master/man/figures/collateral_example.png)

`collateral` uses `pillar` to style output, so supported terminals will also color the output! Properly styling knitted output is still on the to-do list, though.

## Support

If you have a problem with `collateral`, please don't hesitate to [file an issue](https://github.com/rensa/collateral/issues/new) or [get in touch with me](twitter.com/rensa_co)!
