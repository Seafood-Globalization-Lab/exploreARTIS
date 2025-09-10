# NOTE: In all test datasets below, we always include both "domestic" and
#       "foreign" rows (even if one has consumption = 0). This is necessary
#       because the create_summary_consumption() function assumes both exist
#       and will otherwise error during rename().

library(testthat)
library(dplyr)
library(tidyr)

# Test 1: Typical case with both "domestic" and "foreign" sources
test_that("create_summary_consumption returns correct structure with both sources", {
  # Create a minimal input dataset with both domestic and foreign consumption
  df <- tibble(
    iso3c = c("USA", "USA"),
    hs6 = c("010121", "010121"),
    sciname = c("Salmo salar", "Salmo salar"),
    habitat = c("marine", "marine"),
    method = c("wild", "wild"),
    year = c(2020, 2020),
    dom_source = c("domestic", "foreign"),
    consumption_live_t = c(50, 30)
  )
  
  # Call the function
  result <- create_summary_consumption(df)
  
  # Confirm output is a tibble (tidyverse standard)
  expect_true(tibble::is_tibble(result))
  
  # Check required output columns exist
  expected_cols <- c(
    "iso3c", "hs6", "sciname", "habitat", "method", "year",
    "domestic_consumption_t", "foreign_consumption_t", "supply"
  )
  expect_true(all(expected_cols %in% names(result)))
  
  # Check that the numeric values match expected logic
  # Domestic = 50, Foreign = 30, Supply = 80
  expect_equal(result$domestic_consumption_t, 50)
  expect_equal(result$foreign_consumption_t, 30)
  expect_equal(result$supply, 80)
})

# Test 2: Only "domestic" source present (foreign = 0)
test_that("create_summary_consumption handles case with only domestic consumption", {
  # Input contains only domestic consumption
  df <- tibble(
    iso3c = "USA",
    hs6 = "010121",
    sciname = "Salmo salar",
    habitat = "marine",
    method = "wild",
    year = 2020,
    dom_source = "domestic",
    consumption_live_t = 100
  ) %>%
    # Explicitly add a foreign row with 0 to avoid rename() error inside function
    bind_rows(tibble(
      iso3c = "USA",
      hs6 = "010121",
      sciname = "Salmo salar",
      habitat = "marine",
      method = "wild",
      year = 2020,
      dom_source = "foreign",
      consumption_live_t = 0
    ))
  
  result <- create_summary_consumption(df)
  
  # Domestic = 100, Foreign = 0, Supply = 100
  expect_equal(result$domestic_consumption_t, 100)
  expect_equal(result$foreign_consumption_t, 0)
  expect_equal(result$supply, 100)
})

# Test 3: Only "foreign" source present (domestic = 0)
test_that("create_summary_consumption handles case with only foreign consumption", {
  # Input contains only foreign consumption
  df <- tibble(
    iso3c = "CAN",
    hs6 = "020202",
    sciname = "Oncorhynchus mykiss",
    habitat = "fresh",
    method = "farm",
    year = 2021,
    dom_source = "foreign",
    consumption_live_t = 75
  ) %>%
    # Explicitly add a domestic row with 0
    bind_rows(tibble(
      iso3c = "CAN",
      hs6 = "020202",
      sciname = "Oncorhynchus mykiss",
      habitat = "fresh",
      method = "farm",
      year = 2021,
      dom_source = "domestic",
      consumption_live_t = 0
    ))
  
  result <- create_summary_consumption(df)
  
  # Domestic = 0, Foreign = 75, Supply = 75
  expect_equal(result$domestic_consumption_t, 0)
  expect_equal(result$foreign_consumption_t, 75)
  expect_equal(result$supply, 75)
})

# Test 4: per_capita_limit parameter triggers limit_per_capita_consumption()
test_that("create_summary_consumption applies per_capita_limit if provided", {
  # Example input with both domestic and foreign rows
  df <- tibble(
    iso3c = c("USA", "USA"),
    hs6 = "010121",
    sciname = "Salmo salar",
    habitat = "marine",
    method = "wild",
    year = 2020,
    dom_source = c("domestic", "foreign"),
    consumption_live_t = c(100, 50)
  )
  
  # Mock function: halves the consumption values (pretend this is limit_per_capita_consumption)
  mock_fun <- function(df, per_capita_limit) {
    df$consumption_live_t <- df$consumption_live_t / 2
    df
  }
  
  # Use testthat helper to temporarily override limit_per_capita_consumption
  with_mocked_bindings(
    result <- create_summary_consumption(df, per_capita_limit = 200),
    limit_per_capita_consumption = mock_fun
  )
  
  # Domestic should be halved: 100 → 50
  # Foreign should be halved: 50 → 25
  # Supply = 75
  expect_equal(result$domestic_consumption_t, 50)
  expect_equal(result$foreign_consumption_t, 25)
  expect_equal(result$supply, 75)
})
