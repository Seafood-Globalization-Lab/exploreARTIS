# Create sample data sets

library(DBI)
library(tidyverse)
library(countrycode)

# Read in regions 
load("R/sysdata.rda")

# Database connection
con <- dbConnect(RPostgres::Postgres(),
                 dbname=Sys.getenv("DB_NAME"),
                 host="localhost",
                 port="5432",
                 user=Sys.getenv("DB_USERNAME"),
                 password=Sys.getenv("DB_PASSWORD"))

# Check that connection is established by checking which tables are present
dbListTables(con)

# ARTIS data frame
artis_query <- "SELECT * FROM snet WHERE 
((hs_version = 'HS12' AND year >= 2015 AND year <= 2020))"

artis <- dbGetQuery(con, artis_query) %>%
  select(-record_id) %>%
  filter(!is.na(live_weight_t) & !is.na(product_weight_t)) %>%
  mutate(hs6 = as.character(hs6)) %>%
  mutate(hs6 = case_when(
    str_length(hs6) == 5 ~ paste("0", hs6, sep = ""),
    TRUE ~ hs6
  ))

mini_artis <- artis %>%
  filter(live_weight_t > 1000) %>%
  left_join(owid_regions, by = c("source_country_iso3c" = "code")) %>%
  group_by(year, region) %>%
  slice_sample(n = 10)

save(mini_artis, file = "data/mini_artis.rda")

# consumption data frame
consumption_query <- "SELECT * FROM consumption WHERE 
((hs_version = 'HS12' AND year >= 2015 AND year <= 2020))"

consumption <- dbGetQuery(con, consumption_query) %>%
  select(-record_id) 

mini_consumption <- consumption %>%
  filter(consumption_live_t > 1000) %>%
  left_join(owid_regions, by = c("consumer_iso3c" = "code")) %>%
  group_by(year, region) %>%
  slice_sample(n = 10)

save(mini_consumption, file = "data/mini_consumption.rda")
