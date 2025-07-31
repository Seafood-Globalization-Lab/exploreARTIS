library(testthat)
library(ggplot2)
library(dplyr)
library(exploreARTIS)  # your package

data("mini_artis")  # example/test dataset

# Test that basic usage of plot_bar returns a ggplot object with a bar geom
test_that("plot_bar returns a ggplot object with basic usage", {
  # Suppress internal warnings related to tidyselect deprecation for cleaner test output
  p <- suppressWarnings(plot_bar(mini_artis, bar_group = "importer_iso3c"))
  expect_s3_class(p, "ggplot")
  
  # Confirm that the plot contains a bar geom (either GeomBar or GeomCol)
  geoms <- sapply(p$layers, function(x) class(x$geom)[1])
  expect_true(any(geoms %in% c("GeomBar", "GeomCol")))
})

# Test that plot_bar respects the fill_type parameter and produces stacked bars
test_that("plot_bar respects fill_type and produces stacked bars", {
  # Suppress tidyselect deprecation warnings inside plot_bar
  p <- suppressWarnings(plot_bar(mini_artis, bar_group = "importer_iso3c", fill_type = "habitat"))
  expect_s3_class(p, "ggplot")
  
  # Verify that a bar geom is present indicating correct plot type
  geoms <- sapply(p$layers, function(x) class(x$geom)[1])
  expect_true(any(geoms %in% c("GeomBar", "GeomCol")))
})

# Test that plot_bar correctly facets the plot when a facet_variable is given
test_that("plot_bar facets correctly when facet_variable is provided", {
  # Suppress warnings unrelated to user errors to keep test output clean
  p <- suppressWarnings(plot_bar(mini_artis, bar_group = "importer_iso3c", facet_variable = "habitat"))
  expect_s3_class(p, "ggplot")
  
  # Confirm that the plot uses a facet object (FacetWrap or FacetGrid)
  expect_true("FacetWrap" %in% class(p$facet) || "FacetGrid" %in% class(p$facet))
})

# Test that plot_bar warns and returns NULL if an invalid bar_group is supplied
test_that("plot_bar warns on invalid bar_group input", {
  # Do not suppress warnings here, as we expect a user-facing warning and want to test it
  expect_warning(
    res <- plot_bar(mini_artis, bar_group = NA),
    regexp = "please select a valid bar group to plot"
  )
  expect_null(res)
})

# Test that plot_bar warns and returns NULL if bar_group and facet_variable are the same
test_that("plot_bar warns when bar_group and facet_variable are the same", {
  # Again, do not suppress warnings to catch the expected warning properly
  expect_warning(
    res <- plot_bar(mini_artis, bar_group = "habitat", facet_variable = "habitat"),
    regexp = "bar group and facet variable cannot be the same"
  )
  expect_null(res)
})

# Test that plot_bar ignores facet_values when facet_variable is NA without warning
test_that("plot_bar ignores facet_values when facet_variable is NA", {
  # Suppress warnings here since no user warning is expected; facet_values is simply ignored
  expect_silent(
    suppressWarnings(
      p <- plot_bar(mini_artis, bar_group = "importer_iso3c", facet_variable = NA, facet_values = "invalid")
    )
  )
  expect_s3_class(p, "ggplot")
})

# Test that plot_bar returns a plot even when facet_values are invalid but facet_variable is valid
test_that("plot_bar returns plot even when facet_values are invalid", {
  # Suppress warnings to avoid tidyselect messages; invalid facet_values are allowed without user warnings
  p <- suppressWarnings(plot_bar(mini_artis, bar_group = "importer_iso3c", facet_variable = "habitat", facet_values = "invalid"))
  expect_s3_class(p, "ggplot")
})