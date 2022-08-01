#' @import tidyverse
#' @import countrycode
#' @import circlize
#' @export

plot_chord <- function(data, focal_country = NA,
                       species = NA, years = NA,
                       producers = NA, exporters = NA, importers = NA,
                       hs_codes = NA, prod_method = NA, prod_environment = NA,
                       export_source = NA, 
                       weight = "live"){
  # Select live or product weight
  if(weight == "live"){
    quantity <- "live_weight_t"
    quantity.lab <- "Quantity (t live weight)"
  }else{
    quantity <- "product_weight_t"
    quantity.lab <- "Quantity (t product weight)"
  }
  
  # Filter to data selection
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
      filter(., environment %in% prod_environment)
      else .} %>%
    {if (sum(is.na(export_source)) == 0)
      filter(., dom_source %in% export_source)
      else .} 
  
  # Set colors
  # Define Oceana color palette
  # Sector and link colors for focal ISO both imports and exports
  iso_all_trade <- "black" # Navy
  
  # Sector colors for all regions
  e_asia <- "#142B58" # Navy 
  s_asia <- "#792560" # Purple
  lat_amer <- "#F35D2D" #Other option: "#13808F" # Teal
  n_amer <- "#39A584" # Sea Green
  mid_east <- "#C78F0B"# Gold
  ss_afr <- "#355936" # Forest Green
  euro <- "#0686E5" # Light Blue "#00FFFF" # Cyan 
  
  if(sum(is.na(focal_country)) > 0){
    # Less transparent when no focal country is selected
    trans_value <- "80"
  }else{
    # For exports not destined for ISO: set to 40% transparency
    trans_value <- "40"
  }

  e_asia_other <- paste(e_asia, trans_value, sep = "")
  s_asia_other <- paste(s_asia, trans_value, sep = "")
  lat_amer_other <- paste(lat_amer, trans_value, sep = "")
  n_amer_other <- paste(n_amer, trans_value, sep = "")
  mid_east_other <- paste(mid_east, trans_value, sep = "")
  ss_afr_other <- paste(ss_afr, trans_value, sep = "")
  euro_other <- paste(euro, trans_value, sep = "")
  
  # Add region columns
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
  
  country_to_region <- get_country_to_region_trade(data) %>%
    abbrev_region() 

    chordDiagram(country_to_region,
                 grid.col = sector_color_fun(country_to_region, country_iso = focal_country),
                 col = link_transparency_fun(country_to_region, country_iso = focal_country),
                 order = sector_order_fun(country_to_region, country_iso = focal_country),
                 annotationTrack = c("name", "grid"),
                 directional = 1,
                 direction.type = c("diffHeight", "arrows"),
                 link.arr.type = "big.arrow",
                 link.target.prop = FALSE,
                 diffHeight = 0.08,
                 link.sort = TRUE)
}
