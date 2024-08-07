% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_ts.R
\name{plot_ts}
\alias{plot_ts}
\title{Creates a time series line graph/stacked area graph}
\usage{
plot_ts(
  data,
  artis_var = NA,
  plot.type = "line",
  prop_flow_cutoff = 0.05,
  value = "live_weight_t",
  facet_variable = NA,
  facet_values = NA,
  y.lab = NA,
  legend.title = NA,
  plot.title = ""
)
}
\arguments{
\item{data}{an ARTIS trade or consumption data frame.}

\item{artis_var}{variable for color or fill groups.}

\item{plot.type}{select either "line" or "stacked" for the plot type. Default is "line."}

\item{prop_flow_cutoff}{default prop_flow_cutoff = 0.05 means trade volumes that comprise less than 5\% of the total trade are lumped together as "Other".}

\item{value}{trade quantity type to visualize ("live" for live weight or "product" for product weight), default "live."}

\item{facet_variable}{variable name to facet by.}

\item{facet_values}{number of facets or vector of facet categories to include. Must be specifid if a facet_variable is specified.}

\item{y.lab}{text for the y-axis label.}

\item{legend.title}{text for the color or fill legend label.}

\item{plot.title}{text for the plot title.}
}
\description{
Creates a time series line graph/stacked area graph
}
