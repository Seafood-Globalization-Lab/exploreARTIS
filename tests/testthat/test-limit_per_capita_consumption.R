library(testthat)
library(dplyr)
library(exploreARTIS)

# Test 1: Function does not alter data when per capita consumption is already within the limit
test_that("limit_per_capita_consumption does not alter data if within limit", {

  # Create simple test input with low consumption values
  # Population = 100, consumption = 10 t → 100 kg per capita (equal to default limit)
  test_data <- data.frame(
    iso3c = "AAA",
    year = 2020,
    consumption_live_t = 10,
    hs6 = "010101",
    sciname = "Test species",
    habitat = "marine",
    method = "wild",
    source_country_iso3c = "BBB",
    dom_source = "import",
    hs_version = "HS6",
    pop = 100
  )

  # Run the function with default limit
  result <- limit_per_capita_consumption(test_data)

  # Aggregate to check that total consumption is unchanged
  original_total <- sum(test_data$consumption_live_t)
  corrected_total <- sum(result$consumption_live_t)

  # Confirm function does not modify when at or below the limit
  expect_equal(corrected_total, original_total)
})

# Test 2: Function reduces consumption when per capita exceeds the limit
test_that("limit_per_capita_consumption caps per capita consumption above limit", {

  # Population = 100, consumption = 20 t → 200 kg per capita (above default limit of 100)
  test_data <- data.frame(
    iso3c = "BBB",
    year = 2020,
    consumption_live_t = 20,
    hs6 = "010101",
    sciname = "Test species",
    habitat = "marine",
    method = "wild",
    source_country_iso3c = "CCC",
    dom_source = "import",
    hs_version = "HS6",
    pop = 100
  )

  # Run the function
  result <- limit_per_capita_consumption(test_data)

  # Expected capped total = 100 kg * 100 pop / 1000 (kg → t) = 10 t
  expected_total <- 10
  corrected_total <- sum(result$consumption_live_t)

  # Confirm function reduces value correctly
  expect_equal(corrected_total, expected_total)
})

# Test 3: Function respects custom per capita limit
test_that("limit_per_capita_consumption respects custom per capita limit", {

  # Population = 100, consumption = 20 t → 200 kg per capita
  test_data <- data.frame(
    iso3c = "CCC",
    year = 2020,
    consumption_live_t = 20,
    hs6 = "010101",
    sciname = "Test species",
    habitat = "marine",
    method = "wild",
    source_country_iso3c = "DDD",
    dom_source = "import",
    hs_version = "HS6",
    pop = 100
  )

  # Use a stricter limit of 150 kg per capita
  result <- limit_per_capita_consumption(test_data, per_capita_limit = 150)

  # Expected capped total = 150 kg * 100 pop / 1000 (kg → t) = 15 t
  expected_total <- 15
  corrected_total <- sum(result$consumption_live_t)

  # Confirm function applies the stricter custom limit
  expect_equal(corrected_total, expected_total)
})