#' Creates a line graph import or export partners from ARTIS data
#' 
#' This is a function that creates a line graph of traded species with one line per species.
#' 
#' @param data an ARTIS dataframe.
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
#' @return None
#' @import tidyverse
#' @import countrycode
#' @import viridis
#' @export

plot_prod_method_line <- function(data,
                              species = NA, years = NA,
                              producers = NA, exporters = NA, importers = NA,
                              hs_codes = NA, prod_method = NA, prod_environment = NA,
                              export_source = NA, 
                              weight = "live",
                              plot.title = ""){
  # data should be an ARTIS data frame
  # Default prop_flow_cutoff = 0.05 means trade volumes that comprise less than 5% 
  # of the total trade are lumped together as "Other"
  
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

  data <- data %>%
    group_by(year, method) %>%
    summarise(quantity = sum(.data[[quantity]], na.rm = TRUE)) %>%
    ungroup()
  
  # Create full list of partners and years 
  method_year_grid <- expand_grid(year = unique(data$year),
                                  method = unique(data$method))
  
  # reorder levels for graph
  data$method <- factor(data$method, levels = c("aquaculture", "capture", "unknown"))
  
  # Bind stacked line graph data to grid so that zeroes can be filled in for "missing" years
  # Need to do this for time series to plot correctly
  data %>%
    full_join(method_year_grid, by = c("year", "method")) %>%
    mutate(quantity = if_else(is.na(quantity), true = 0, false = quantity)) %>%
    ggplot() +
    geom_line(aes(x = year, y = quantity, color = method), size = 1.1) +
    scale_color_manual(values = c("#741A32", "#114F59", "#D38F35")) +
    labs(y = quantity.lab, x = "Year", title = plot.title, color = "Production Method") +
    theme_bw() 
}
