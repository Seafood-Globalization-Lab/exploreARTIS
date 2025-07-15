library(testthat)
library(dplyr)
library(stringr)

# If your function is not loaded via package, uncomment and adjust the path:
# source("path/to/your_function.R")

# Test 1: Verify that calculate_supply returns the correct columns and structure with typical input data
test_that("calculate_supply returns correct columns and basic structure", {
  # Create example artis_data with mixed export types and species/method/habitat
  artis_data <- tibble(
    year = c(2020, 2020, 2020, 2020),
    exporter_iso3c = c("USA", "USA", "USA", "CAN"),
    importer_iso3c = c("CAN", "MEX", "USA", "USA"),
    dom_source = c("domestic export", "foreign export", "error export", "domestic export"),
    sciname = c("FishA", "FishA", "FishA", "FishB"),
    method = c("wild", "wild", "wild", "farm"),
    habitat = c("marine", "marine", "marine", "fresh"),
    live_weight_t = c(100, 50, 10, 200)
  )
  
  # Create example production_data corresponding to artis_data species and regions
  production_data <- tibble(
    year = c(2020, 2020),
    iso3c = c("USA", "CAN"),
    sciname = c("FishA", "FishB"),
    method = c("wild", "farm"),
    habitat = c("marine", "fresh"),
    live_weight_t = c(500, 300)
  )
  
  # Call the function with test inputs
  result <- calculate_supply(artis_data, production_data)
  
  # Check the function returns a tibble (preferred data structure for tidyverse)
  expect_true(tibble::is_tibble(result))
  
  # Check that all expected columns are present in the output
  expected_cols <- c(
    "year", "iso3c", "sciname", "method", "habitat", 
    "domestic_export", "error_export", "foreign_export", 
    "imports_live_weight_t", "production_t", 
    "supply", "supply_no_error", "supply_domestic", "supply_foreign"
  )
  expect_true(all(expected_cols %in% colnames(result)))
  
  # Verify that no NA values remain in key numeric columns after processing
  expect_false(any(is.na(result$domestic_export)))
  expect_false(any(is.na(result$foreign_export)))
  expect_false(any(is.na(result$error_export)))
  expect_false(any(is.na(result$imports_live_weight_t)))
  expect_false(any(is.na(result$production_t)))
  
  # Select a specific row to validate calculation correctness
  usa_fisha <- result %>% 
    filter(iso3c == "USA", sciname == "FishA", method == "wild", habitat == "marine")
  
  # Manually compute expected values for this subset:
  # production_t = 500 (from production_data)
  # imports_live_weight_t = 10 (error export import to USA)
  # domestic_export = 100 (domestic export from USA)
  # foreign_export = 50 (foreign export from USA)
  # error_export = 10 (error export from USA)
  # supply = production + imports - domestic_export - foreign_export - error_export
  # supply = 500 + 10 - 100 - 50 - 10 = 350
  
  expect_equal(usa_fisha$production_t, 500)
  expect_equal(usa_fisha$imports_live_weight_t, 10)
  expect_equal(usa_fisha$domestic_export, 100)
  expect_equal(usa_fisha$foreign_export, 50)
  expect_equal(usa_fisha$error_export, 10)
  expect_equal(usa_fisha$supply, 350)
})

# Test 2: Check how calculate_supply behaves with empty input data
test_that("calculate_supply handles missing data gracefully", {
  # Create empty artis_data tibble with proper column names and types
  artis_data <- tibble(
    year = integer(),
    exporter_iso3c = character(),
    importer_iso3c = character(),
    dom_source = character(),
    sciname = character(),
    method = character(),
    habitat = character(),
    live_weight_t = numeric()
  )
  
  # Create empty production_data tibble with matching columns
  production_data <- tibble(
    year = integer(),
    iso3c = character(),
    sciname = character(),
    method = character(),
    habitat = character(),
    live_weight_t = numeric()
  )
  
  # Call calculate_supply with empty inputs, suppress warnings due to missing columns inside the function
  result <- suppressWarnings(calculate_supply(artis_data, production_data))
  
  # Check that result is still a tibble (maintains consistent return type)
  expect_true(tibble::is_tibble(result))
  
  # Confirm the result has zero rows, indicating graceful handling of empty inputs
  expect_equal(nrow(result), 0)
  
  # We do not check for columns or NAs here because no rows exist and some columns might be missing
  # This avoids warnings about unknown columns in empty results
})