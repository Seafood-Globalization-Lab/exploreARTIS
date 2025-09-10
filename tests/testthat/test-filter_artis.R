library(testthat)
library(dplyr)

# Test 1: Verify that filter_artis correctly filters by species
test_that("filter_artis filters by species correctly", {
  # Create a small example dataset with multiple species
  data <- tibble(
    sciname = c("FishA", "FishB", "FishA", "FishC"),
    year = c(2020, 2021, 2021, 2020),
    source_country_iso3c = c("USA", "CAN", "USA", "MEX"),
    exporter_iso3c = c("USA", "CAN", "USA", "MEX"),
    importer_iso3c = c("MEX", "USA", "CAN", "CAN"),
    hs6 = c("123456", "123456", "654321", "654321"),
    method = c("wild", "farm", "wild", "farm"),
    habitat = c("marine", "fresh", "marine", "fresh"),
    dom_source = c("domestic export", "foreign export", "domestic export", "foreign export")
  )
  
  # Call the function with a filter for only FishA
  result <- filter_artis(data, species = "FishA")
  
  # Expect all rows to contain only "FishA"
  expect_true(all(result$sciname == "FishA"))
  
  # Expect number of rows to match the two FishA entries in the input
  expect_equal(nrow(result), 2)
})

# Test 2: Verify filtering by multiple criteria simultaneously
test_that("filter_artis handles multiple filters together", {
  # Use the same dataset as above
  data <- tibble(
    sciname = c("FishA", "FishB", "FishA", "FishC"),
    year = c(2020, 2021, 2021, 2020),
    source_country_iso3c = c("USA", "CAN", "USA", "MEX"),
    exporter_iso3c = c("USA", "CAN", "USA", "MEX"),
    importer_iso3c = c("MEX", "USA", "CAN", "CAN"),
    hs6 = c("123456", "123456", "654321", "654321"),
    method = c("wild", "farm", "wild", "farm"),
    habitat = c("marine", "fresh", "marine", "fresh"),
    dom_source = c("domestic export", "foreign export", "domestic export", "foreign export")
  )
  
  # Apply a filter for species = FishA and year = 2021
  result <- filter_artis(data, species = "FishA", years = 2021)
  
  # Expect only rows that are FishA AND year = 2021
  expect_true(all(result$sciname == "FishA"))
  expect_true(all(result$year == 2021))
  
  # Expect only one row should match (FishA + 2021)
  expect_equal(nrow(result), 1)
})

# Test 3: Check filtering with NA arguments (should return unfiltered data)
test_that("filter_artis returns full dataset when all filters are NA", {
  # Create a very small dataset
  data <- tibble(
    sciname = c("FishA", "FishB"),
    year = c(2020, 2021),
    source_country_iso3c = c("USA", "CAN"),
    exporter_iso3c = c("USA", "CAN"),
    importer_iso3c = c("MEX", "USA"),
    hs6 = c("123456", "654321"),
    method = c("wild", "farm"),
    habitat = c("marine", "fresh"),
    dom_source = c("domestic export", "foreign export")
  )
  
  # Call filter_artis without applying any filters (all arguments = NA by default)
  result <- filter_artis(data)
  
  # The result should match the original dataset exactly
  expect_equal(nrow(result), nrow(data))
  expect_equal(colnames(result), colnames(data))
})

# Test 4: Verify filtering by exporters and importers
test_that("filter_artis filters correctly by exporters and importers", {
  # Create example dataset
  data <- tibble(
    sciname = c("FishA", "FishB", "FishC"),
    year = c(2020, 2021, 2020),
    source_country_iso3c = c("USA", "CAN", "MEX"),
    exporter_iso3c = c("USA", "CAN", "MEX"),
    importer_iso3c = c("MEX", "USA", "CAN"),
    hs6 = c("111111", "222222", "333333"),
    method = c("wild", "farm", "wild"),
    habitat = c("marine", "fresh", "marine"),
    dom_source = c("domestic export", "foreign export", "domestic export")
  )
  
  # Filter to only keep rows where exporter = CAN and importer = USA
  result <- filter_artis(data, exporters = "CAN", importers = "USA")
  
  # Expect the result to only include FishB, which matches exporter=CAN and importer=USA
  expect_equal(result$sciname, "FishB")
  expect_equal(nrow(result), 1)
})

# Test 5: Verify filtering by production method and habitat
test_that("filter_artis filters by production method and environment", {
  # Create example dataset
  data <- tibble(
    sciname = c("FishA", "FishB", "FishC"),
    year = c(2020, 2020, 2021),
    source_country_iso3c = c("USA", "CAN", "MEX"),
    exporter_iso3c = c("USA", "CAN", "MEX"),
    importer_iso3c = c("MEX", "USA", "CAN"),
    hs6 = c("444444", "555555", "666666"),
    method = c("wild", "farm", "wild"),
    habitat = c("marine", "fresh", "fresh"),
    dom_source = c("domestic export", "foreign export", "domestic export")
  )
  
  # Filter for farmed species in fresh environments
  result <- filter_artis(data, prod_method = "farm", prod_environment = "fresh")
  
  # Only FishB meets the criteria (farm + fresh)
  expect_equal(result$sciname, "FishB")
  expect_equal(nrow(result), 1)
})

# Test 6: Verify filtering by export source
test_that("filter_artis filters by export source correctly", {
  # Create example dataset
  data <- tibble(
    sciname = c("FishA", "FishB"),
    year = c(2020, 2021),
    source_country_iso3c = c("USA", "CAN"),
    exporter_iso3c = c("USA", "CAN"),
    importer_iso3c = c("MEX", "USA"),
    hs6 = c("777777", "888888"),
    method = c("wild", "farm"),
    habitat = c("marine", "fresh"),
    dom_source = c("domestic export", "foreign export")
  )
  
  # Apply filter for only foreign exports
  result <- filter_artis(data, export_source = "foreign export")
  
  # Expect only FishB to remain
  expect_equal(result$sciname, "FishB")
  expect_equal(result$dom_source, "foreign export")
})