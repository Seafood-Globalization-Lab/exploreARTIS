# Script for testing function

# Load packages
#library(DBI)
#library(tidyverse) # try and reduce size of dependencies
library(dplyr)
library(exploreARTIS)
library(countrycode)
library(sf)
library(rnaturalearth)
library(CoordinateCleaner)
library(circlize)
library(ggsankey)
library(ggpubr)

# switch testing to `mini_artis` and `mini_consumption` exploreARTIS dataframes

# con <- dbConnect(RPostgres::Postgres(),
#                  dbname=Sys.getenv("POSTGRES_DB"),
#                  host="localhost",
#                  port="5432",
#                  user=Sys.getenv("POSTGRES_USER"),
#                  password=Sys.getenv("POSTGRES_PASSWORD"))
# 
# # Check that connection is established by checking which tables are present
# dbListTables(con)
# 
# # Pull all ARTIS and production data
# artis <- dbGetQuery(con, "SELECT * FROM snet WHERE product_weight_t > 10000") %>%
#   select(-record_id)
# 
# consumption <- dbGetQuery(con, "SELECT * FROM complete_consumption WHERE consumption_live_t > 10000") %>%
#   select(-record_id)
# 
# prod <- dbGetQuery(con, "SELECT * FROM production") %>%
#   select(-record_id)
# 
# countries <- dbGetQuery(con, "SELECT * FROM countries") %>%
#   select(-record_id)
# 
# # Close database connection
# dbDisconnect(con)
# 
# rm(list = c("con")) 


# regional_artis <- read.csv("/Volumes/jgephart/ARTIS/Outputs/S_net/snet_20220928/regional_snet.csv")
regional_artis <- mini_artis %>%
  left_join(
    countries %>%
      select(exporter_iso3c = iso3c, exporter_region = owid_region),
    by = c("exporter_iso3c")
  ) %>%
  left_join(
    countries %>%
      select(importer_iso3c = iso3c, importer_region = owid_region),
    by = c("importer_iso3c")
  ) %>%
  group_by(exporter_region, importer_region, method, habitat, year) %>%
  summarize(live_weight_t = sum(live_weight_t, na.rm = TRUE))

# Test general line graph function----------------------------------------------
plot_ts(mini_artis, artis_var = "method", plot.title = "Testing Title 2")
plot_ts(mini_artis, artis_var = "hs6", plot.title = "Testing HS Products 2", prop_flow_cutoff = 0.05)
plot_ts(mini_artis, artis_var = "sciname", plot.title = "Testing HS Products 2", prop_flow_cutoff = 0.05, plot.type = "stacked")
plot_ts(mini_artis, artis_var = "exporter_iso3c", plot.title = "Testing Exporters", prop_flow_cutoff = 0.05)
plot_ts(mini_artis, artis_var = "exporter_iso3c", plot.title = "Testing Exporters Stacked", prop_flow_cutoff = 0.05, plot.type = "stacked")

plot_ts(mini_artis,
        artis_var = "hs6", facet_variable = "sciname",
        plot.type = "stacked", facet_values = 6, prop_flow_cutoff = 0.1)

plot_ts(mini_artis,
        artis_var = "sciname", facet_variable = "hs6",
        plot.type = "stacked", facet_values = 6, prop_flow_cutoff = 0.1)

plot_ts(mini_artis,
        artis_var = "sciname", facet_variable = "method",
        plot.type = "stacked", facet_values = 6, prop_flow_cutoff = 0.1)

plot_ts(mini_artis,
        artis_var = "method", facet_variable = "method",
        plot.type = "stacked", facet_values = 6, prop_flow_cutoff = 0.1)

# Testing plot_bar
plot_bar(mini_artis, bar_group = "importer_iso3c")
plot_bar(mini_artis %>%
           filter(sciname %in% c("salmo salar", "engraulis ringens")), bar_group = "importer_iso3c")


plot_bar(mini_artis, bar_group = "importer_iso3c", fill_type = "method") 
plot_bar(mini_artis, bar_group = "exporter_iso3c", fill_type = "method")
plot_bar(mini_artis, bar_group = "sciname", fill_type = "method")
plot_bar(mini_artis, bar_group = "importer_iso3c", fill_type = "dom_source")
plot_bar(mini_artis, bar_group = "exporter_iso3c", fill_type = "dom_source")
plot_bar(mini_artis, bar_group = "sciname", fill_type = "dom_source")
plot_bar(mini_artis, bar_group = "exporter_iso3c", regions = TRUE)
plot_bar(mini_artis, bar_group = "exporter_iso3c", regions = TRUE, fill_type = "dom_source")
plot_bar(mini_artis, bar_group = "importer_iso3c", regions = TRUE)

plot_bar(mini_artis,
        bar_group = "hs6", facet_variable = "sciname", facet_n = 6)

plot_bar(mini_artis,
         bar_group = "sciname", facet_variable = "hs6", facet_n = 6,
         fill_type = "method")

plot_bar(mini_artis,
         bar_group = "hs6", facet_variable = "sciname", facet_n = 6,
         fill_type = "method")

plot_bar(mini_artis,
        bar_group = "sciname", facet_variable = "hs6", facet_n = 5)

plot_bar(mini_artis,
        bar_group = "sciname", facet_variable = "method", facet_n = 4)



# Test sankey function----------------------------------------------------------
plot_sankey(mini_artis)
plot_sankey(mini_artis, cols = c("method", 
                            "source_country_iso3c", 
                            "exporter_iso3c"))
plot_sankey(mini_artis, cols = c("method", 
                            "source_country_iso3c",
                            "importer_iso3c",
                            "exporter_iso3c"))
plot_sankey(mini_artis, cols = c("habitat",
                            "method", 
                            "source_country_iso3c",
                            "importer_iso3c",
                            "exporter_iso3c"))
plot_sankey(mini_artis, cols = c("habitat",
                            "method", 
                            "source_country_iso3c"),
            show.other = FALSE)
plot_sankey(mini_consumption, cols = c("source_country_iso3c",
                                  "exporter_iso3c",
                                  "consumer_iso3c"),
            value = "consumption_live_t") 
mini_consumption %>%
  filter_artis(species = "salmo salar") %>%
  plot_sankey(cols = c("source_country_iso3c", 
                       "consumer_iso3c"),
              value = "consumption_live_t",
              show.other = TRUE) 

mini_artis %>%
  filter_artis(species = "salmo salar") %>%
  plot_sankey(cols = c("source_country_iso3c", 
                       "importer_iso3c"),
              value = "live_weight_t",
              show.other = FALSE, prop_flow_cutoff = 0.05) 

mini_artis %>%
  add_region(col = "importer_iso3c") %>%
  add_region(col = "source_country_iso3c") %>%
  plot_sankey(cols = c("source_country_iso3c_region", "importer_iso3c_region"))

# Test regional sankey function-------------------------------------------------
plot_regional_sankey_method_habitat(regional_artis, 1996, 2019)
plot_regional_sankey_method_habitat(regional_artis, 2019, 2019)

# Testing mapping function
mini_artis %>% 
  filter(exporter_iso3c == "KIR") %>%
  plot_map(country_fill = "importer_iso3c", flow_arrows = TRUE, n_flows = 10)

# Testing mapping for regions
mini_artis %>%
  plot_map(country_fill = "export", flow_arrows = TRUE, regions = TRUE)

# Testing plot_chord
plot_chord(mini_artis, years = 2016, region_colors = region7_palette)
plot_chord(mini_artis, 
           years = 2000, 
           prod_method = "capture", 
           focal_country = "USA", 
           region_colors = region7_palette)
plot_chord(mini_artis, 
           years = 2018, 
           prod_method = "capture", 
           focal_country = "USA", 
           region_colors = region7_palette)
plot_chord(mini_artis, 
           years = 2000, 
           prod_method = "aquaculture", 
           focal_country = "USA", 
           region_colors = region7_palette)
plot_chord(mini_artis, 
           years = 2018, 
           prod_method = "aquaculture", 
           focal_country = "USA", 
           region_colors = region7_palette)

plot_chord(mini_artis, 
           years = 2016, 
           focal_country = c("USA", "CHN"), 
           region_colors = region7_palette)

# Test plot_chord for region
plot_chord(regional_artis, 
           years = 2016, 
           prod_method = "capture", 
           prod_environment = "marine", 
           plot_region = TRUE, 
           region_colors = region6_palette)

# Testing calculate_supply
supply <- calculate_supply(mini_artis, prod)

# Negative supply
supply_negative_summary <- supply %>% 
  group_by(iso3c, year) %>%
  mutate(total_supply = sum(supply_domestic)) %>%
  group_by(iso3c, sciname, year, total_supply) %>%
  summarise(supply = sum(supply_domestic)) %>%
  arrange(supply)

