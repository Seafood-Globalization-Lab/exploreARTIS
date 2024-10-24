% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add_country_name.R
\name{add_country_name}
\alias{add_country_name}
\title{Convert country ISO3c codes to name column}
\usage{
add_country_name(data, col, country.col.name = NA)
}
\arguments{
\item{data}{dataframe. Must contain chr column of ISO3c codes}

\item{col}{character. Column name within data that contains ISO3c codes.}

\item{country.col.name}{character. User specified name of new country name column. Default will take input col and add "_name" for the new column name.}
}
\description{
`add_country_name()` takes an ISO 3166-1 alpha-3 codes column and creates a new column with full country names using OWID table. "NEI" (i.e. nowhere else included) column values are replaced with "Nowhere Else Included" and "unknown" values are replaced with "Unknown". Will return an error if "country_name" column already exists in the dataframe.
}
\examples{

# Use `mini_artis` dataframe included in package, remove "country_name" column
library(dplyr)
test_data <- mini_artis \%>\% select(importer_iso3c, year, region, live_weight_t)

# Convert importer column codes to new column of full names
glimpse(test_data \%>\% 
          add_country_name("importer_iso3c"))

# Provide new column name
glimpse(test_data \%>\% 
          add_country_name("importer_iso3c",
                            country.col.name = "test_column_name"))

}
