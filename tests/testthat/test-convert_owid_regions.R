library(testthat)
library(dplyr)

# Test that convert_owid_regions returns a tibble with the expected structure
test_that("convert_owid_regions returns tibble with expected structure", {
  # Create a minimal owid_regions mapping (3 known codes)
  owid_regions <<- tibble::tibble(
    code = c("USA", "CAN", "MEX"),
    region = c("North America", "North America", "North America")
  )
  
  # Input data includes both known codes and an unknown "XXX"
  df <- tibble::tibble(
    exporter_iso3c = c("USA", "CAN", "XXX"),
    importer_iso3c = c("MEX", "XXX", "USA"),
    hs6 = "010121",
    sciname = "Fish sp.",
    habitat = "Marine",
    method = "Wild",
    hs_version = "2012",
    year = c(2020, 2020, 2020),
    product_weight_t = c(10, 20, 30),
    live_weight_t = c(100, 200, 300)
  )
  
  res <- convert_owid_regions(df)
  
  # Confirm output is a tibble and contains required columns
  expect_s3_class(res, "tbl_df")
  expect_true(all(c("exporter_region", "importer_region",
                    "product_weight_t", "live_weight_t") %in% names(res)))
})

# Test that unknown ISO3 codes are correctly mapped to "Other"
test_that("convert_owid_regions maps unknown codes to Other", {
  # Use same owid_regions mapping (no "XXX" defined)
  owid_regions <<- tibble::tibble(
    code = c("USA", "CAN", "MEX"),
    region = c("North America", "North America", "North America")
  )
  
  # Input data with only unknown codes
  df <- tibble::tibble(
    exporter_iso3c = "XXX",
    importer_iso3c = "XXX",
    hs6 = "010121",
    sciname = "Fish sp.",
    habitat = "Marine",
    method = "Wild",
    hs_version = "2012",
    year = 2020,
    product_weight_t = 10,
    live_weight_t = 100
  )
  
  res <- convert_owid_regions(df)
  
  # Confirm that both exporter and importer map to "Other"
  expect_true("Other" %in% res$exporter_region)
  expect_true("Other" %in% res$importer_region)
})

# Test that product_weight_t and live_weight_t are aggregated correctly
test_that("convert_owid_regions correctly aggregates weights", {
  # owid_regions mapping where both codes belong to the same region
  owid_regions <<- tibble::tibble(
    code = c("USA", "CAN"),
    region = c("North America", "North America")
  )
  
  # Input data with two rows that should be aggregated
  df <- tibble::tibble(
    exporter_iso3c = c("USA", "CAN"),
    importer_iso3c = c("USA", "CAN"),
    hs6 = "010121",
    sciname = "Fish sp.",
    habitat = "Marine",
    method = "Wild",
    hs_version = "2012",
    year = c(2020, 2020),
    product_weight_t = c(50, 70),
    live_weight_t = c(500, 700)
  )
  
  res <- convert_owid_regions(df)
  
  # Confirm aggregation is correct
  expect_equal(sum(res$product_weight_t), 120)
  expect_equal(sum(res$live_weight_t), 1200)
})

# Test that empty input returns an empty tibble with correct columns
test_that("convert_owid_regions handles empty input", {
  # Empty owid_regions mapping
  owid_regions <<- tibble::tibble(code = character(), region = character())
  
  # Empty input data
  df <- tibble::tibble(
    exporter_iso3c = character(),
    importer_iso3c = character(),
    hs6 = character(),
    sciname = character(),
    habitat = character(),
    method = character(),
    hs_version = character(),
    year = integer(),
    product_weight_t = numeric(),
    live_weight_t = numeric()
  )
  
  res <- convert_owid_regions(df)
  
  # Confirm output has 0 rows but still includes required columns
  expect_equal(nrow(res), 0)
  expect_true(all(c("exporter_region", "importer_region",
                    "product_weight_t", "live_weight_t") %in% names(res)))
})

# Test that when all countries are known, no "Other" regions are assigned
test_that("convert_owid_regions does not assign Other when all codes are valid", {
  # owid_regions mapping with all input codes defined
  owid_regions <<- tibble::tibble(
    code = c("USA", "CAN", "MEX"),
    region = c("North America", "North America", "North America")
  )
  
  # Input data with only valid codes
  df <- tibble::tibble(
    exporter_iso3c = c("USA", "CAN"),
    importer_iso3c = c("MEX", "USA"),
    hs6 = "010121",
    sciname = "Fish sp.",
    habitat = "Marine",
    method = "Wild",
    hs_version = "2012",
    year = c(2020, 2020),
    product_weight_t = c(50, 70),
    live_weight_t = c(500, 700)
  )
  
  res <- convert_owid_regions(df)
  
  # Confirm that "Other" is not present in either exporter or importer regions
  expect_false("Other" %in% res$exporter_region)
  expect_false("Other" %in% res$importer_region)
})
