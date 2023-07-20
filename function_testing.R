# Script for testing function

# Load packages
library(DBI)
library(tidyverse)
library(exploreARTIS)
library(countrycode)
library(sf)
library(rnaturalearth)
library(CoordinateCleaner)
library(circlize)
library(ggsankey)
library(ggpubr)


con <- dbConnect(RPostgres::Postgres(),
                 dbname=Sys.getenv("POSTGRES_DB"),
                 host="localhost",
                 port="5432",
                 user=Sys.getenv("POSTGRES_USER"),
                 password=Sys.getenv("POSTGRES_PASSWORD"))

# Check that connection is established by checking which tables are present
dbListTables(con)

# Pull all ARTIS and production data
artis <- dbGetQuery(con, "SELECT * FROM snet WHERE product_weight_t > 10000") %>%
  select(-record_id)

prod <- dbGetQuery(con, "SELECT * FROM production") %>%
  select(-record_id)

countries <- dbGetQuery(con, "SELECT * FROM countries") %>%
  select(-record_id)

# Close database connection
dbDisconnect(con)

rm(list = c("con")) 


# regional_artis <- read.csv("/Volumes/jgephart/ARTIS/Outputs/S_net/snet_20220928/regional_snet.csv")
regional_artis <- artis %>%
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
plot_ts(artis, artis_var = "method", plot.title = "Testing Title 2")
plot_ts(artis, artis_var = "hs6", plot.title = "Testing HS Products 2", prop_flow_cutoff = 0.05)
plot_ts(artis, artis_var = "sciname", plot.title = "Testing HS Products 2", prop_flow_cutoff = 0.05, plot.type = "stacked")
plot_ts(artis, artis_var = "exporter_iso3c", plot.title = "Testing Exporters", prop_flow_cutoff = 0.05)
plot_ts(artis, artis_var = "exporter_iso3c", plot.title = "Testing Exporters Stacked", prop_flow_cutoff = 0.05, plot.type = "stacked")
plot_ts(artis, artis_var = "exporter_iso3c", plot.title = "Testing Exporter Regions", regions = "owid")

plot_ts(artis,
        artis_var = "hs6", facet_variable = "sciname",
        plot.type = "stacked", facet_values = 6, prop_flow_cutoff = 0.1)

plot_ts(artis,
        artis_var = "sciname", facet_variable = "hs6",
        plot.type = "stacked", facet_values = 6, prop_flow_cutoff = 0.1)

plot_ts(artis,
        artis_var = "sciname", facet_variable = "method",
        plot.type = "stacked", facet_values = 6, prop_flow_cutoff = 0.1)

plot_ts(artis,
        artis_var = "method", facet_variable = "method",
        plot.type = "stacked", facet_values = 6, prop_flow_cutoff = 0.1)

# Testing plot_bar
plot_bar(artis, bar_group = "importer_iso3c")
plot_bar(artis %>%
           filter(sciname %in% c("salmo salar", "engraulis ringens")), bar_group = "importer_iso3c")
plot_bar(artis, bar_group = "importer_iso3c", fill_type = "method")
plot_bar(artis, bar_group = "exporter_iso3c", fill_type = "method")
plot_bar(artis, bar_group = "sciname", fill_type = "method")
plot_bar(artis, bar_group = "importer_iso3c", fill_type = "dom_source")
plot_bar(artis, bar_group = "exporter_iso3c", fill_type = "dom_source")
plot_bar(artis, bar_group = "sciname", fill_type = "dom_source")
plot_bar(artis, bar_group = "exporter_iso3c", regions = TRUE)
plot_bar(artis, bar_group = "exporter_iso3c", regions = TRUE, fill_type = "dom_source")
plot_bar(artis, bar_group = "importer_iso3c", regions = TRUE)

plot_bar(artis,
        bar_group = "hs6", facet_variable = "sciname", facet_n = 6)

plot_bar(artis,
         bar_group = "sciname", facet_variable = "hs6", facet_n = 6,
         fill_type = "method")

plot_bar(artis,
         bar_group = "hs6", facet_variable = "sciname", facet_n = 6,
         fill_type = "method")

plot_bar(artis,
        bar_group = "sciname", facet_variable = "hs6", facet_n = 5)

plot_bar(artis,
        bar_group = "sciname", facet_variable = "method", facet_n = 4)



# Test sankey function----------------------------------------------------------
plot_sankey(artis)
plot_sankey(artis, regions = "owid")
plot_sankey(artis, regions = "region23")

# Test regional sankey function-------------------------------------------------
plot_regional_sankey_method_habitat(regional_artis, 1996, 2019)
plot_regional_sankey_method_habitat(regional_artis, 2019, 2019)

# Testing mapping function
artis %>% 
  filter(exporter_iso3c == "KIR") %>%
  plot_map(country_fill = "importer_iso3c", flow_arrows = TRUE, n_flows = 10)

# Testing mapping for regions
artis %>%
  plot_map(country_fill = "export", flow_arrows = TRUE, regions = TRUE)

# Testing plot_chord
plot_chord(artis, years = 2016, region_colors = region7_palette)
plot_chord(artis, years = 2000, prod_method = "capture", focal_country = "USA", 
           region_colors = region7_palette)
plot_chord(artis, years = 2018, prod_method = "capture", focal_country = "USA", 
           region_colors = region7_palette)
plot_chord(artis, years = 2000, prod_method = "aquaculture", focal_country = "USA", 
           region_colors = region7_palette)
plot_chord(artis, years = 2018, prod_method = "aquaculture", focal_country = "USA", 
           region_colors = region7_palette)

plot_chord(artis, years = 2016, focal_country = c("USA", "CHN"), region_colors = region7_palette)

# Test plot_chord for region
plot_chord(regional_artis, years = 2016, 
           prod_method = "capture", prod_environment = "marine", 
           plot_region = TRUE, region_colors = region6_palette)

# Testing calculate_supply
supply <- calculate_supply(artis, prod)

# Negative supply
supply_negative_summary <- supply %>% 
  group_by(iso3c, year) %>%
  mutate(total_supply = sum(supply_domestic)) %>%
  group_by(iso3c, sciname, year, total_supply) %>%
  summarise(supply = sum(supply_domestic)) %>%
  arrange(supply)

# Old functions (legacy code) ##################################################

# Test plot_partner_line function-----------------------------------------------
# plot_partner_line(artis, trade_flow = "import", prop_flow_cutoff = 0.05)
# plot_partner_line(artis, trade_flow = "export")
# 
# plot_partner_line(artis, trade_flow = "import", weight = "live")
# plot_partner_line(artis, trade_flow = "import", weight = "product")
# 
# plot_partner_line(artis, trade_flow = "export", regions = "owid")
# plot_partner_line(artis, trade_flow = "export", regions = "region23")

# Test plot_partner_stacked function--------------------------------------------
# plot_partner_stacked(artis, trade_flow = "import", prop_flow_cutoff = 0.05)
# plot_partner_stacked(artis, trade_flow = "export")
# 
# plot_partner_stacked(artis, trade_flow = "export", regions = "owid")
# plot_partner_stacked(artis, trade_flow = "export", regions = "region23")
# plot_partner_stacked(artis, trade_flow = "export", regions = "region")
# 
# plot_partner_stacked(artis, species = "salmo salar", trade_flow = "import")
# plot_partner_stacked(artis, species = "salmo salar", trade_flow = "export")
# 
# 
# plot_partner_stacked(artis, trade_flow = "import", species = c("salmo salar", "thunnus albacares"))
# plot_partner_stacked(artis, trade_flow = "export", species = c("salmo salar", "thunnus albacares"))
# 
# plot_partner_stacked(artis, trade_flow = "import", years = 2012:2018)
# plot_partner_stacked(artis, trade_flow = "export", years = 2012:2018)
# 
# plot_partner_stacked(artis, trade_flow = "import", producers = c("CHN", "MEX"))
# plot_partner_stacked(artis, trade_flow = "export", producers = c("CHN", "MEX"))
# 
# plot_partner_stacked(artis, trade_flow = "import", exporters = c("CHN", "MEX"))
# plot_partner_stacked(artis, trade_flow = "export", exporters = c("CHN", "MEX"))
# 
# plot_partner_stacked(artis, trade_flow = "import", importers = c("CHN", "MEX"))
# plot_partner_stacked(artis, trade_flow = "export", importers = c("CHN", "MEX"))
# 
# plot_partner_stacked(artis, trade_flow = "import", hs_codes = c("230120", "030613"))
# plot_partner_stacked(artis, trade_flow = "export", hs_codes = c("230120", "030613"))
# 
# plot_partner_stacked(artis, trade_flow = "import", prod_method = "capture", prod_environment = "marine")
# plot_partner_stacked(artis, trade_flow = "export", prod_method = "capture", prod_environment = "marine")
# 
# plot_partner_stacked(artis, trade_flow = "import", export_source = "foreign export")
# plot_partner_stacked(artis, trade_flow = "export", export_source = "foreign export")

# # Test plot_species_line function
# plot_species_line(artis, common_names = TRUE)
# plot_species_line(artis, species = c("salmo salar", "thunnus albacares"))

# # Test plot_species_stacked function
# plot_species_stacked(artis)
# plot_species_stacked(artis, common_names = TRUE)
# 
# plot_species_stacked(artis, species = c("salmo salar", "thunnus albacares"))
# 
# plot_species_stacked(artis, years = 2012:2018)
# 
# plot_species_stacked(artis, producers = c("CHN", "MEX"))
# 
# plot_species_stacked(artis, hs_codes = c("230120", "030613"))
# 
# # Test plot_export_source_line function
# plot_export_source_line(artis)
# 
# # Test plot_export_source_stacked function
# plot_export_source_stacked(artis)
# 
# # Test plot_prod_method_line function
# plot_hs_product_line(artis)
# 
# # Test plot_prod_method_stacked function
# plot_hs_product_stacked(artis)
# 
# # Test plot_prod_method_line function
# plot_prod_method_line(artis)
# 
# # Test plot_prod_method_stacked function
# plot_prod_method_stacked(artis)





