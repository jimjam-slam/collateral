context("Mapping functions return correctly structured data")

library(tibble)
library(dplyr)
library(tidyr)
library(purrr)
library(collateral)

# test data -------------------------------------------------------------------

test_list_data <- list(1:5, 'apple', -6, 17)
test_list_second <- list('red', c('yellow', 'blue'), 'green', NA)

# test data frame
test_spiked <-
  mtcars %>%
  rownames_to_column(var = "car") %>%
  as_tibble() %>%
  select(car, cyl, disp, wt) %>%
  # negate some rows in cyl == 4 to make them fail
  mutate(wt = dplyr::case_when(
    wt < 2 ~ -wt,
    TRUE ~ wt))

test_dfs <- test_spiked %>% nest(data = -cyl) %>% pull(data)
test_subset <- test_spiked %>% dplyr::filter(cyl == 4)

# map variant structure tests -------------------------------------------------

test_that("map_safely correctly structures output", {
  expect_type(
    map_safely(test_dfs, ~ log(.$wt)),
    'list')
  expect_s3_class(
    map_safely(test_dfs, ~ log(.$wt)),
    'safely_mapped')
  walk(
    map_safely(test_dfs, ~ log(.$wt)),
    expect_named,
    ignore.order = TRUE,
    expected = c('result', 'error'))
})

test_that("map_quietly correctly structures output", {
  expect_type(
    map_quietly(test_dfs, ~ log(.$wt)),
    'list')
  expect_s3_class(
    map_quietly(test_dfs, ~ log(.$wt)),
    'quietly_mapped')
  walk(
    map_quietly(test_dfs, ~ log(.$wt)),
    expect_named,
    ignore.order = TRUE,
    expected = c('result', 'output', 'warnings', 'messages'))
})

test_that("map_peacefully correctly structures output", {
  expect_type(
    map_peacefully(test_dfs, ~ log(.$wt)),
    'list')
  expect_s3_class(
    map_peacefully(test_dfs, ~ log(.$wt)),
    'peacefully_mapped')
  map(
    map_peacefully(test_dfs, ~ log(.$wt)),
    expect_named,
    ignore.order = TRUE,
    expected = c('result', 'error', 'warnings', 'messages', 'output'))
})

# map2 variant structure tests ------------------------------------------------

test_that("map2_safely correctly structures output", {
  expect_type(
    map2_safely(test_subset$cyl, test_subset$wt, ~ .x * log(.y)),
    'list')
  expect_s3_class(
    map2_safely(test_subset$cyl, test_subset$wt, ~ .x * log(.y)),
    'safely_mapped')
  map(
    map2_safely(test_subset$cyl, test_subset$wt, ~ .x * log(.y)),
    expect_named,
    ignore.order = TRUE,
    expected = c('result', 'error'))
})

test_that("map2_quietly correctly structures output", {
  expect_type(
    map2_quietly(test_subset$cyl, test_subset$wt, ~ .x * log(.y)),
    'list')
  expect_s3_class(
    map2_quietly(test_subset$cyl, test_subset$wt, ~ .x * log(.y)),
    'quietly_mapped')
  walk(
    map2_quietly(test_subset$cyl, test_subset$wt, ~ .x * log(.y)),
    expect_named,
    ignore.order = TRUE,
    expected = c('result', 'output', 'warnings', 'messages'))
})

test_that("map2_peacefully correctly structures output", {
  expect_type(
    map2_peacefully(test_subset$cyl, test_subset$wt, ~ .x * log(.y)),
    'list')
  expect_s3_class(
    map2_peacefully(test_subset$cyl, test_subset$wt, ~ .x * log(.y)),
    'peacefully_mapped')
  map(
    map2_peacefully(test_subset$cyl, test_subset$wt, ~ .x * log(.y)),
    expect_named,
    ignore.order = TRUE,
    expected = c('result', 'error', 'warnings', 'messages', 'output'))
})

# pmap variant structure tests ------------------------------------------------

test_that("pmap_safely correctly structures output", {
  expect_type(
    pmap_safely(
      list(test_subset$cyl, test_subset$wt, test_subset$disp),
      ~ ..1 * log(..2) / ..3),
    'list')
  expect_s3_class(
    pmap_safely(
      list(test_subset$cyl, test_subset$wt, test_subset$disp),
      ~ ..1 * log(..2) / ..3),
    'safely_mapped')
  map(
    pmap_safely(
      list(test_subset$cyl, test_subset$wt, test_subset$disp),
      ~ ..1 * log(..2) / ..3),
    expect_named,
    ignore.order = TRUE,
    expected = c('result', 'error'))
})

test_that("pmap_quietly correctly structures output", {
  expect_type(
    pmap_quietly(
      list(test_subset$cyl, test_subset$wt, test_subset$disp),
      ~ ..1 * log(..2) / ..3),
    'list')
  expect_s3_class(
    pmap_quietly(
      list(test_subset$cyl, test_subset$wt, test_subset$disp),
      ~ ..1 * log(..2) / ..3),
    'quietly_mapped')
  walk(
    pmap_quietly(
      list(test_subset$cyl, test_subset$wt, test_subset$disp),
      ~ ..1 * log(..2) / ..3),
    expect_named,
    ignore.order = TRUE,
    expected = c('result', 'output', 'warnings', 'messages'))
})

test_that("pmap_peacefully correctly structures output", {
  expect_type(
    pmap_peacefully(
      list(test_subset$cyl, test_subset$wt, test_subset$disp),
      ~ ..1 * log(..2) / ..3),
    'list')
  expect_s3_class(
    pmap_peacefully(
      list(test_subset$cyl, test_subset$wt, test_subset$disp),
      ~ ..1 * log(..2) / ..3),
    'peacefully_mapped')
  walk(
    pmap_peacefully(
      list(test_subset$cyl, test_subset$wt, test_subset$disp),
      ~ ..1 * log(..2) / ..3),
    expect_named,
    ignore.order = TRUE,
    expected = c('result', 'output', 'warnings', 'messages', 'error'))
})

# TODO - setup tests for future_map_* variants
# (probably skipping on ci and cran)
