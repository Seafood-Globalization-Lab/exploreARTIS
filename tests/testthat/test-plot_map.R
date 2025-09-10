library(testthat)
library(ggplot2)
library(dplyr)
library(exploreARTIS)  # your package

data("mini_artis")

# Dummy datasets for testing
dummy_data <- tibble(
  importer_iso3c = c("USA", "CHN", "FRA", "USA"),
  exporter_iso3c = c("BRA", "USA", "CHN", "FRA"),
  live_weight_t = c(100, 200, 150, 50)
)

country_centroids <- tibble(
  iso3 = c("USA", "CHN", "FRA", "BRA"),
  centroid.lat = c(38, 35, 46, -10),
  centroid.lon = c(-97, 104, 2, -55)
)

owid_regions <- tibble(
  code = c("USA", "CHN", "FRA", "BRA"),
  region = c("Americas", "Asia", "Europe", "Americas")
)

owid_centroids <- tibble(
  region = c("Americas", "Asia", "Europe"),
  centroid.lat = c(20, 35, 50),
  centroid.lon = c(-80, 105, 10)
)

# Test that plot_map returns a ggplot object with default parameters
test_that("plot_map returns a ggplot object with default parameters", {
  p <- plot_map(dummy_data)
  expect_s3_class(p, "ggplot")
})

# Test that plot_map returns a ggplot object when country_fill is used
test_that("plot_map returns a ggplot object when country_fill is used", {
  p <- plot_map(dummy_data, country_fill = "importer_iso3c")
  expect_s3_class(p, "ggplot")
})

# Test that plot_map returns a ggplot object when flow_arrows = TRUE
test_that("plot_map returns a ggplot object when flow arrows are TRUE", {
  p <- plot_map(dummy_data, flow_arrows = TRUE)
  expect_s3_class(p, "ggplot")
})

# Test that plot_map returns a ggplot object when using regions
test_that("plot_map returns a ggplot object when using regions", {
  p <- plot_map(dummy_data, country_fill = "importer_iso3c", flow_arrows = TRUE, regions = TRUE)
  expect_s3_class(p, "ggplot")
})

# Test that plot_map correctly applies caption_label
test_that("plot_map handles captions correctly", {
  p <- plot_map(dummy_data, caption_label = "Test caption")
  expect_s3_class(p, "ggplot")
  expect_true("Test caption" %in% p$labels$caption)
})

# Test that plot_map handles empty or NA data gracefully
test_that("plot_map handles empty or NA data gracefully", {
  empty_data <- tibble(importer_iso3c = character(), exporter_iso3c = character(), live_weight_t = numeric())
  p <- plot_map(empty_data)
  expect_s3_class(p, "ggplot")
})
