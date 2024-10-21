# Standardize SAU data with ARTIS
# Join Prod SAU & ARTIS SAU data 


# Clean Data -------------------------------------------------
prod_sau <- prod_sau %>%
  
  # clarify column names
  rename(prod_iso3 = country_iso3_alpha,
         catch_eez = eez) %>% 
  
  # Break apart catch_eez column - identify ISO3 codes with one of the 3 eez columns
  separate(catch_eez, 
           into = c("catch_eez_1", "catch_eez_2"), 
           sep = "\\(", remove = FALSE) %>%
  mutate(catch_eez_2 = gsub("\\)", "", catch_eez_2)) %>%
  
  # create new cleaned eez column - eez_iso3 - from countrycode library names
  mutate(catch_eez_iso3c = countrycode(catch_eez, 
                                       origin = "country.name", 
                                       destination = "iso3c")) %>% 
  mutate(catch_eez_iso3c = case_when(
    (!is.na(catch_eez_iso3c)) ~ catch_eez_iso3c, 
    (is.na(catch_eez_iso3c) ~ countrycode(catch_eez_1, 
                                          origin = "country.name", 
                                          destination = "iso3c")))) %>%
  mutate(catch_eez_iso3c = case_when(
    (!is.na(catch_eez_iso3c)) ~ catch_eez_iso3c, 
    (is.na(catch_eez_iso3c) ~ countrycode(catch_eez_2, 
                                          origin = "country.name", 
                                          destination = "iso3c")))) %>%
  
  # correct single instance 
  mutate(catch_eez_iso3c = case_when(
    (catch_eez == "US Virgin Isl.") ~ "USA",
    TRUE ~ catch_eez_iso3c
  )) %>%
  
  # use cleaned ISO3 codes to generate standard country name column
  mutate(catch_eez_name = countrycode(catch_eez_iso3c, 
                                      origin = "iso3c", 
                                      destination = "country.name")) %>% 
  
  # adds artis_iso3 and catch_artis_country_name columns
  standardize_sau_eez("catch_eez_iso3c", "catch_eez_name") %>% 
  rename(catch_artis_iso3 = artis_iso3,
         catch_artis_country_name = artis_country_name) %>% 
  
  # remove columns only used for standardization process
  select(-catch_eez_1, -catch_eez_2, -catch_eez) %>% 
  
  # aggregate catch quantity records
  # group_by(group_by(across(-quantity))) %>% 
  # summarize(live_weight_t = sum(quantity)) %>% 
  group_by(year, prod_iso3, SciName, prod_method, 
           habitat, catch_artis_iso3, catch_artis_country_name) %>% 
  summarise(live_weight_t = sum(quantity)) %>% 
  ungroup() %>% 
  
  # Tag domestic versus foreign fishing
  mutate(dwf = case_when(
    (catch_artis_iso3 == prod_iso3) ~ "domestic",
    TRUE ~ "foreign"
  ))

# Join Prod SAU & ARTIS SAU data --------------------------------------------

# Proportion of landings by country flag captured in recorded source eezs
prod_sau_props <- prod_sau %>%
  # aggregate landings amount - 
  # disregard habitat, production method, sector, end use
  group_by(year, prod_iso3, SciName, 
           catch_artis_country_name, catch_artis_iso3, dwf) %>%
  summarise(live_weight_t = sum(live_weight_t)) %>% 
  # calculate prop catch over each source eez - 
  # does not contract df over 2nd group_by()
  group_by(year, prod_iso3, SciName) %>% # needs to be exactly what data is joining by after
  mutate(prop_by_catch_eez = live_weight_t/sum(live_weight_t)) %>%
  select(-live_weight_t)

# Disaggregate ARTIS by EEZ of catch - join datasets
consumption_eez <- consumption %>% 
  # prod_sau (therefor prod_sau_props) is inherently only marine capture - match artis_sau data
  filter(habitat == "marine", 
         method == "capture") %>%
  # pull prod_sau_props data for year year, source country, and species
  left_join(prod_sau_props, 
            by = c("year", 
                   "source_country_iso3c" = "prod_iso3", 
                   "sciname" = "SciName"), 
            relationship = "many-to-many") %>% 
  # recalculate live_weight_t catch - each trade and product record gets split apart by the number of catch eez from prod_sau_props - essentially assigning a probability a product was caught in a specific eez. 
  mutate(live_weight_t = consumption_t*prop_by_catch_eez) %>% 
  # many-to-many warning is what we expect here - one row of artis_sau correlates with multiple prod_sau eez
  group_by(year, 
           catch_artis_iso3, 
           catch_artis_country_name,
           source_country_iso3c, 
           consumer_iso3c, 
           sciname, 
           dwf) %>% 
  summarise(live_weight_t = sum(live_weight_t))
