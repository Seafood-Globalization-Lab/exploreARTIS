% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_bar.R
\name{plot_bar}
\alias{plot_bar}
\title{ARTIS Bar Chart}
\usage{
plot_bar(
  data,
  bar_group,
  value = "live_weight_t",
  fill_type = NA,
  top_n = 10,
  y.lab = "",
  x.lab = NA,
  fill.lab = "",
  plot.title = "",
  facet_variable = NA,
  facet_n = NA
)
}
\arguments{
\item{data}{an ARTIS dataframe.}

\item{bar_group}{refers to which column for the bars that will be visualized (ie exporter_iso3c, importer_iso3c, sciname).}

\item{value}{column name for the values the bar length should represent.}

\item{fill_type}{column to use to stack the bars (ie by method, habitat).}

\item{top_n}{number of the top ranked groups to plot.}

\item{y.lab}{text for the y-axis label.}

\item{x.lab}{text for the x-axis label.}

\item{fill.lab}{text for the fill legend label.}

\item{plot.title}{text for the plot title.}

\item{facet_variable}{variable name to facet by.}

\item{facet_n}{number of facets to include. Must be specifid if a facet_variable is specified.}
}
\description{
Function that creates a bar chart for ARTIS data.
}
