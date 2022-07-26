% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_sankey.R
\name{plot_sankey}
\alias{plot_sankey}
\title{Creates a Sankey plot of ARTIS data}
\usage{
plot_sankey(
  data,
  prop_flow_cutoff = 0.05,
  species = NA,
  years = NA,
  producers = NA,
  exporters = NA,
  importers = NA,
  hs_codes = NA,
  prod_method = NA,
  prod_environment = NA,
  export_source = NA,
  weight = "live"
)
}
\arguments{
\item{data}{an ARTIS dataframe.}

\item{prop_flow_cutoff}{default prop_flow_cutoff = 0.05 means trade volumes that comprise less than 5\% of the total trade are lumped together as "Other".}

\item{species}{list of species/species groups to include, default NA - includes all species.}

\item{years}{list of years to include, default NA - includes all years.}

\item{producers}{list of producers (as iso3 codes) to include, default NA - includes all producers.}

\item{exporters}{list of exporters (as iso3 codes) to include, default NA - includes all exporters.}

\item{importers}{list of importers (as iso3 codes) to include, default NA - includes all importers.}

\item{hs_codes}{list of hs level 6 codes to include, default NA - includes all hs6 codes.}

\item{prod_method}{list of production methods (capture, aquaculture, or unknown), default NA - includes all production methods.}

\item{prod_environment}{list of environments (marine, inland, or unknown), default NA - includes all environments}

\item{export_source}{list of types of export (domestic export, foreign export, or error export), default NA - all export sources.}

\item{weight}{trade quantity type to visualize ("live" for live weight or "product" for product weight), default "live."}
}
\value{
None
}
\description{
This is a function that creates a sankey plot showcasing seafood supply chains from producer to importer in the ARTIS dataset.
}
