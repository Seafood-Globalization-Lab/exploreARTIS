
library(tidyverse)
library(CoordinateCleaner)

rm(list = ls())
gc()

# Sector and link colors for focal ISO 
iso_all_trade <- "black" 

# Sector colors for all regions
region7_palette <- c("#142B58", # Navy 
                     "#792560", # Purple
                     "#F35D2D", #Other option: "#13808F" # Teal
                     "#39A584", # Sea Green
                     "#C78F0B",# Gold
                     "#355936", # Forest Green
                     "#0686E5") # Light Blue "#00FFFF" # Cyan

region6_palette <- c(
  "#741A32", # maroon
  "#B34232", # burnt orange
  "#D38F35", #  orange
  "#D4B95F", # khaki
  "#4FA2A2", # teal
  "#114F59" # dark teal
)

artis_palette <- colorRampPalette(region6_palette)

owid_regions <- read.csv("/Volumes/jgephart/ARTIS/Outputs/exploreARTIS_files/owid_regions.csv")
sciname_metadata <- read.csv("/Volumes/jgephart/ARTIS/Outputs/SQL_Database/20220928/sciname.csv")

# Create centroids data frame - for map plot
# FIXIT: Check Kiribati centroid spans east west - point showing up in the wrong place
country_centroids <- countryref %>%
  filter(str_count(as.character(adm1_code))==3) %>%
  group_by(iso3) %>%
  summarise(centroid.lon = mean(centroid.lon),
            centroid.lat = mean(centroid.lat))

countryref <- countryref

# Calculate centroids for regions for OWID
owid_centroids <- owid_regions %>%
  # Selecting specific country centroids for regional centroids
  filter(code == "USA" | # USA - North America
           code == "BRA" | # Brazil - South America
           code == "AUS" | # Australia - Oceania
           code == "CHN" | # China - Asia
           code == "DEU" | # Germany - Europe
           code == "TCD" # Chad - Africa
         ) %>%
  left_join(
    country_centroids,
    by = c("code" = "iso3")
  ) %>%
  select(c(region, centroid.lon, centroid.lat))

# Save all variables for later use by package
save.image(file = "R/sysdata.rda")
