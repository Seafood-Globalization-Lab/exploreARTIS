
library(tidyverse)
library(CoordinateCleaner)

# Sector and link colors for focal ISO 
iso_all_trade <- "black" 

# Sector colors for all regions
e_asia <- "#142B58" # Navy 
s_asia <- "#792560" # Purple
lat_amer <- "#F35D2D" #Other option: "#13808F" # Teal
n_amer <- "#39A584" # Sea Green
mid_east <- "#C78F0B"# Gold
ss_afr <- "#355936" # Forest Green
euro <- "#0686E5" # Light Blue "#00FFFF" # Cyan

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

# Create centroids data frame - for map plot
# FIXIT: Check Kiribati centroid spans east west - point showing up in the wrong place
country_centroids <- countryref %>%
  filter(str_count(as.character(adm1_code))==3) %>%
  group_by(iso3) %>%
  summarise(centroid.lon = mean(centroid.lon),
            centroid.lat = mean(centroid.lat))

countryref <- countryref

# Save all variables for later use by package
save.image(file = "R/sysdata.rda")
