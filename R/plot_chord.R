#' ARTIS chord diagram
#' 
#' Function that creates a chord diagram from ARTIS data.
#' 
#' @param species list of species/species groups to include, default NA - includes all species.
#' @param years list of years to include, default NA - includes all years.
#' @param producers list of producers (as iso3 codes) to include, default NA - includes all producers.
#' @param exporters list of exporters (as iso3 codes) to include, default NA - includes all exporters.
#' @param importers list of importers (as iso3 codes) to include, default NA - includes all importers.
#' @param hs_codes list of hs level 6 codes to include, default NA - includes all hs6 codes.
#' @param prod_method list of production methods (capture, aquaculture, or unknown), default NA - includes all production methods.
#' @param prod_environment list of environments (marine, inland, or unknown), default NA - includes all environments
#' @param export_source list of types of export (domestic export, foreign export, or error export), default NA - all export sources.
#' @param weight trade quantity type to visualize ("live" for live weight or "product" for product weight), default "live."
#' @import tidyverse
#' @import countrycode
#' @import circlize
#' @export

plot_chord <- function(data, focal_country = NA,
                       species = NA, years = NA,
                       producers = NA, exporters = NA, importers = NA, regions = NA,
                       hs_codes = NA, prod_method = NA, prod_environment = NA,
                       export_source = NA, 
                       weight = "live") {
  
  # Setting up parameters based on user input-----------------------------------
  # Select live or product weight
  if(weight == "live"){
    quantity <- "live_weight_t"
    quantity.lab <- "Quantity (t live weight)"
  }else{
    quantity <- "product_weight_t"
    quantity.lab <- "Quantity (t product weight)"
  }
  
  # Transparency value of flows based on focal country
  if(sum(is.na(focal_country)) > 0){
    # Less transparent when no focal country is selected
    trans_value <- "80"
  } else {
    # For exports not destined for ISO: set to 40% transparency
    trans_value <- "40"
  }
  
  # Filtering data based on user input------------------------------------------
  data <- data %>%
    {if (sum(is.na(species)) == 0)
      filter(., sciname %in% species)
      else .} %>%
    {if (sum(is.na(years)) == 0)
      filter(., year %in% years)
      else .} %>%
    {if (sum(is.na(producers)) == 0)
      filter(., source_country_iso3c %in% producers)
      else .} %>%
    {if (sum(is.na(exporters)) == 0)
      filter(., exporter_iso3c %in% exporters)
      else .} %>%
    {if (sum(is.na(importers)) == 0)
      filter(., importer_iso3c %in% importers)
      else .} %>%
    {if (sum(is.na(hs_codes)) == 0)
      filter(., hs6 %in% as.character(as.numeric(hs_codes)))
      else .} %>%
    {if (sum(is.na(prod_method)) == 0)
      filter(., method %in% prod_method)
      else .} %>%
    {if (sum(is.na(prod_environment)) == 0)
      filter(., habitat %in% prod_environment)
      else .} %>%
    {if (sum(is.na(export_source)) == 0)
      filter(., dom_source %in% export_source)
      else .}

  # Adding regional classification----------------------------------------------
  
  if (is.na(regions)) {
    
    # Adding codes 
    data <- data %>%
      left_join(owid_regions %>%
                  select(code, region) %>%
                  rename(exporter_region = region),
                by = c("exporter_iso3c" = "code")) %>%
      left_join(owid_regions %>%
                  select(code, region) %>%
                  rename(importer_region = region),
                by = c("importer_iso3c" = "code"))
  } else {
    
    data <- data %>%
      mutate(importer_region = suppressWarnings(countrycode(importer_iso3c, origin = "iso3c", destination = "region")),
             exporter_region = suppressWarnings(countrycode(exporter_iso3c, origin = "iso3c", destination = "region"))) %>%
      filter(!is.na(importer_region),
             !is.na(exporter_region)) %>%
      # If a focal country is selected, replace the region name with the country iso
      mutate(
        importer_region = case_when((importer_iso3c %in% focal_country) ~ importer_iso3c,
                                    TRUE ~ importer_region),
        
        exporter_region = case_when((exporter_iso3c %in% focal_country) ~ exporter_iso3c,
                                    TRUE ~ exporter_region))
  }
  
  # Re-summarizing data based on regional classification
  country_to_region <- get_country_to_region_trade(data, quantity) %>%
    # Abbreviating region names
    abbrev_region()
  
  # Creating Chord Diagram------------------------------------------------------
  chordDiagram(country_to_region,
               grid.col = sector_color_fun(country_to_region, country_iso = focal_country),
               col = link_transparency_fun(country_to_region, country_iso = focal_country, trans_value),
               order = sector_order_fun(country_to_region, country_iso = focal_country),
               annotationTrack = c("name", "grid"),
               directional = 1,
               direction.type = c("diffHeight", "arrows"),
               link.arr.type = "big.arrow",
               link.target.prop = FALSE,
               diffHeight = 0.08,
               link.sort = TRUE)
}
