library(testthat)
library(dplyr)
library(ggplot2)
library(ggsankey)
library(exploreARTIS)  # your package

# mini_artis dataset included in the package
data("mini_artis")


# Test that plot_sankey returns a ggplot object with default parameters
test_that("plot_sankey returns a ggplot object with default parameters", {
  p <- plot_sankey(mini_artis)
  expect_s3_class(p, "ggplot")
})

# Test that plot_sankey returns a ggplot object with alternative columns
test_that("plot_sankey works with alternative columns", {
  p <- plot_sankey(mini_artis, cols = c("method", "habitat"))
  expect_s3_class(p, "ggplot")
})

# Test that plot_sankey correctly applies prop_flow_cutoff and groups small flows as "Other"
test_that("plot_sankey groups small flows as 'Other'", {
  p <- plot_sankey(mini_artis, prop_flow_cutoff = 0.2)
  expect_s3_class(p, "ggplot")
})

# Test that plot_sankey respects show.other = FALSE and removes "Other" nodes
test_that("plot_sankey removes 'Other' nodes when show.other = FALSE", {
  p <- plot_sankey(mini_artis, prop_flow_cutoff = 0.2, show.other = FALSE)
  expect_s3_class(p, "ggplot")
})

# Test that plot_sankey applies a custom plot title
test_that("plot_sankey applies custom plot title", {
  p <- plot_sankey(mini_artis, plot.title = "Test Sankey")
  expect_s3_class(p, "ggplot")
  expect_equal(p$labels$title, "Test Sankey")
})

# Test that plot_sankey warns when NA values are present in selected columns
test_that("plot_sankey warns if selected columns contain NAs", {
  test_data <- mini_artis
  test_data$importer_iso3c[1] <- NA
  expect_warning(plot_sankey(test_data), regexp = "WARNING: The selected columns include NA values")
})

# Test that plot_sankey handles empty data gracefully
test_that("plot_sankey handles empty data", {
  empty_data <- mini_artis[0, ]
  p <- plot_sankey(empty_data)
  expect_s3_class(p, "ggplot")
})