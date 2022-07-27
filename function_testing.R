# Script for testing function

# Load packages
library(DBI)
library(tidyverse)
library(exploreARTIS)
library(countrycode)

# Load artis data from SQL
# Load ARTIS data from SQL local database
con <- dbConnect(RPostgres::Postgres(),
                 dbname=Sys.getenv("POSTGRES_DB"),
                 host="localhost",
                 port="5432",
                 user=Sys.getenv("POSTGRES_USER"),
                 password=Sys.getenv("POSTGRES_PASSWORD"))

# Check that connection is established by checking which tables are present
dbListTables(con)

# Pull all ARTIS and production data
artis <- dbGetQuery(con, "SELECT * FROM snet") 
artis <- artis %>%
  select(-record_id)

prod <- dbGetQuery(con, "SELECT * FROM production")
prod <- prod %>%
  select(-record_id)

# Close database connection
dbDisconnect(con)

rm(list = c("con"))

# Test plot_partner_stacked function
plot_partner_stacked(artis, trade_flow = "import")
plot_partner_stacked(artis, trade_flow = "export")

plot_partner_stacked(artis, species = "salmo salar", trade_flow = "import")
plot_partner_stacked(artis, species = "salmo salar", trade_flow = "export")


plot_partner_stacked(artis, trade_flow = "import", species = c("salmo salar", "thunnus albacares"))
plot_partner_stacked(artis, trade_flow = "export", species = c("salmo salar", "thunnus albacares"))

plot_partner_stacked(artis, trade_flow = "import", years = 2012:2018)
plot_partner_stacked(artis, trade_flow = "export", years = 2012:2018)

plot_partner_stacked(artis, trade_flow = "import", producers = c("CHN", "MEX"))
plot_partner_stacked(artis, trade_flow = "export", producers = c("CHN", "MEX"))

plot_partner_stacked(artis, trade_flow = "import", exporters = c("CHN", "MEX"))
plot_partner_stacked(artis, trade_flow = "export", exporters = c("CHN", "MEX"))

plot_partner_stacked(artis, trade_flow = "import", importers = c("CHN", "MEX"))
plot_partner_stacked(artis, trade_flow = "export", importers = c("CHN", "MEX"))

plot_partner_stacked(artis, trade_flow = "import", hs_codes = c("230120", "030613"))
plot_partner_stacked(artis, trade_flow = "export", hs_codes = c("230120", "030613"))

plot_partner_stacked(artis, trade_flow = "import", prod_method = "capture", prod_environment = "marine")
plot_partner_stacked(artis, trade_flow = "export", prod_method = "capture", prod_environment = "marine")

plot_partner_stacked(artis, trade_flow = "import", export_source = "foreign export")
plot_partner_stacked(artis, trade_flow = "export", export_source = "foreign export")

