
library(tidyverse)
library(countrycode)

# Preparing conversion data
owid_regions <- read.csv("data/continents-according-to-our-world-in-data.csv")
owid_regions <- owid_regions %>%
  # Getting country names
  mutate(country_name = countrycode(Code, origin = "iso3c", destination = "country.name")) %>%
  # Filter out countries that have NAs
  filter(!is.na(country_name)) %>%
  select(Code, country_name, Continent) %>%
  distinct() %>%
  rename(region = Continent) %>%
  bind_rows(
    data.frame(
      Code = c("SCG"),
      country_name = c("Serbia and Montenegro"),
      region = c("Europe")
    )
  )

# All column names lower case
names(owid_regions) <- tolower(names(owid_regions))

write.csv(owid_regions, "data/owid_regions.csv", row.names = FALSE)

