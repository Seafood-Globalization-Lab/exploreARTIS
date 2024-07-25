# Example script for loading full ARTIS data

# Load packages
library(tidyverse)

# Set data directory where artis data is loaded
datadir <- "outputs/trade"
outdir <- "outputs"

# List files in folder
file_list <- list.files(datadir)

# Select set of years and hs versions of interest
hs_years <- data.frame(
  hs_version = c(
    # Length in each should match the set of selected years
    rep("HS96", length(1996:2003)),
    rep("HS02", length(2004:2009)),
    rep("HS07", length(2010:2012)),
    rep("HS12", length(2013:2020))
    # Note: HS17 is not in the current custom ARTIS, but this would be included
    # if an analysis does include HS17 
    #rep("HS17", length(1996:2003))
  ), 
  year = c(
    # HS96 years
    1996:2003,
    # HS02 years
    2004:2009,
    # HS07 years
    2010:2012,
    # HS12 years
    2013:2020
    # HS17 years
  )) 

# Subset file_list to the set of HS versions and years of interest
file_list <- hs_years %>%
  left_join(as.data.frame(file_list) %>%
              separate(file_list, 
                       into = c("data", "est", "hs_version", "year", "csv")) %>%
              mutate(year = as.integer(year)),
            by = c("hs_version", "year")) %>%
  mutate(file_names = paste(data, est, hs_version, year, sep = "_")) %>%
  mutate(file_names = paste0(file_names, ".csv")) %>%
  pull(file_names)

# Loop through selected files and filter each column as desired
# before building back up the file for analysis

# Create empty data frame to build up
df <- data.frame(hs_version = c(), year = c(), 
                 source_country_iso3c = c(), exporter_iso3c = c(), 
                 importer_iso3c = c(), dom_source = c(), 
                 hs6 = c(), sciname = c(), habitat = c(), method = c(), 
                 product_weight_t = c(), live_weight_t = c())

for(i in 1:3){
  
  print(paste(i, "of", length(file_list), file_list[i], sep = " "))
  
  df_i <- read.csv(file.path(datadir, file_list[i])) %>%
    # Source country
    # Uncomment and add any iso3c codes to filter by
    #filter(source_country_iso3c %in% c()) %>%
    
    # Exporter
    # Uncomment and add any iso3c codes to filter by
    #filter(exporter_iso3c %in% c()) %>%
    
    # Importer
    # Uncomment and add any iso3c codes to filter by
    #filter(importer_iso3c %in% c()) %>%
    
    # HS6 code
    # Add lead zeros to filter hs6 codes 
    # (do this regardless of whether you filter by hs6 codes)
    mutate(hs6 = as.character(hs6)) %>%
    mutate(hs6 = case_when(
      str_length(hs6) == 5 ~ paste("0", hs6, sep = ""),
      TRUE ~ hs6
    )) %>%
    # Uncomment and add any hs6 codes to filter by
    #filter(hs6 %in% c()) %>%
    
    # Export source (domestic vs foreign vs unknown)
    # Uncomment and add any export sources to filter by
    #filter(dom_source %in% c()) %>%
    
    # Scientific names
    # Uncomment and add any scientific names to filter by
    #filter(sciname %in% c()) %>%
    
    # Habitat
    # Uncomment and add any habitats to filter by
    #filter(habitat %in% c()) %>%
    
    # Method
    # Uncomment and add any methods to filter by
    #filter(method %in% c()) %>%
    select(hs_version, year, 
           source_country_iso3c, exporter_iso3c, importer_iso3c, dom_source, 
           hs6, sciname, habitat, method, 
           product_weight_t, live_weight_t)
  
  df <- df %>%
    bind_rows(df_i)
}

write.csv(df, file.path(outdir, "filtered_artis.csv"), row.names = FALSE)  

