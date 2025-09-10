library(testthat)
library(dplyr)
library(ggplot2)
library(forcats)
library(exploreARTIS)  # your package

data("mini_artis")  # dataset in package


# Test that plot_ts returns a ggplot object with default parameters
test_that("plot_ts returns a ggplot object with default parameters", {
  # Default line plot for a valid artis_var
  p <- plot_ts(mini_artis, artis_var = "importer_iso3c")
  expect_s3_class(p, "ggplot")
})

# Test that plot_ts returns a ggplot object when using stacked area plot
test_that("plot_ts returns a ggplot object for stacked plot.type", {
  p <- plot_ts(mini_artis, artis_var = "importer_iso3c", plot.type = "stacked")
  expect_s3_class(p, "ggplot")
})

# Test that plot_ts warns and returns NULL if artis_var is NA
test_that("plot_ts warns if artis_var is not provided", {
  expect_warning(res <- plot_ts(mini_artis), regexp = "please select a variable to plot")
  expect_null(res)
})

# Test that plot_ts warns and returns NULL if artis_var equals facet_variable
test_that("plot_ts warns when artis_var equals facet_variable", {
  expect_warning(res <- plot_ts(mini_artis, artis_var = "habitat", facet_variable = "habitat"), 
                 regexp = "artis_var cannot be the same as facet_variable")
  expect_null(res)
})

# Test that plot_ts applies custom plot title and axis labels
test_that("plot_ts applies custom plot.title and y.lab", {
  p <- plot_ts(mini_artis, artis_var = "importer_iso3c", plot.title = "Custom Title", y.lab = "Custom Y")
  expect_s3_class(p, "ggplot")
  expect_equal(p$labels$title, "Custom Title")
  expect_equal(p$labels$y, "Custom Y")
})

# Test that plot_ts handles facet_variable with specified facet_values
test_that("plot_ts correctly facets by facet_variable and facet_values", {
  p <- plot_ts(mini_artis, artis_var = "importer_iso3c", facet_variable = "habitat", facet_values = c("Marine", "Freshwater"))
  expect_s3_class(p, "ggplot")
  expect_true("FacetWrap" %in% class(p$facet))
})

# Test that plot_ts applies prop_flow_cutoff and renames low-proportion variables to "Other"
test_that("plot_ts renames low-proportion variables as 'Other'", {
  p <- plot_ts(mini_artis, artis_var = "importer_iso3c", prop_flow_cutoff = 0.2)
  expect_s3_class(p, "ggplot")
})

# Test that plot_ts handles empty data gracefully
test_that("plot_ts handles empty data", {
  empty_data <- mini_artis[0, ]
  p <- plot_ts(empty_data, artis_var = "importer_iso3c")
  expect_s3_class(p, "ggplot")
})
