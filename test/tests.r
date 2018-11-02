library(tidyverse)
library(collateral)

test =
  # tidy up and trim down for the example
  mtcars %>%
  tibble::rownames_to_column(var = "car") %>%
  tibble::as_data_frame() %>%
  dplyr::select(car, cyl, disp, wt) %>%
  # spike some rows in cyl == 4 to make them fail
  dplyr::mutate(wt = dplyr::case_when(
    wt < 2 ~ -wt,
    TRUE ~ wt)) %>%
  # nest and do some operations quietly()
  tidyr::nest(-cyl) %>%
  dplyr::mutate(
    qlog = map_quietly(data, ~ log(.$wt)),
    slog = map_safely(data, ~ log(.$wt)))

test
