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

plot_export_source_line <- function(data,
                                  species = NA, years = NA,
                                  producers = NA, exporters = NA, importers = NA,
                                  hs_codes = NA, prod_method = NA, prod_environment = NA,
                                  export_source = NA, 
                                  weight = "live",
                                  plot.title = ""){

  # Setting up parameters based on user input-----------------------------------
  # Select live or product weight
  if(weight == "live"){
    quantity <- "live_weight_t"
    quantity.lab <- "Quantity (t live weight)"
  }else{
    quantity <- "product_weight_t"
    quantity.lab <- "Quantity (t product weight)"
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
      filter(., environment %in% prod_environment)
      else .} %>%
    {if (sum(is.na(export_source)) == 0)
      filter(., dom_source %in% export_source)
      else .} 
  
  # Dom source dataframe and Visualization--------------------------------------
  # Create dataframe of dom source (domestic export, foreign export, etc) by year
  data <- data %>%
    group_by(year, dom_source) %>%
    summarise(quantity = sum(.data[[quantity]], na.rm = TRUE)) %>%
    ungroup()
  
  # Create full list of partners and years 
  dom_source_year_grid <- expand_grid(year = unique(data$year),
                                  dom_source = unique(data$dom_source))
  
  # reorder levels for graph
  data$dom_source <- factor(data$dom_source, levels = c("domestic export", "foreign export", "error export"))
  
  # Bind stacked line graph data to grid so that zeroes can be filled in for "missing" years
  # Need to do this for time series to plot correctly
  data %>%
    full_join(dom_source_year_grid, by = c("year", "dom_source")) %>%
    mutate(quantity = if_else(is.na(quantity), true = 0, false = quantity)) %>%
    # Plot line graph
    ggplot() +
    geom_line(aes(x = year, y = quantity, color = dom_source), size = 1.1) +
    scale_color_manual(values = c("#741A32", "#114F59", "#D38F35")) +
    labs(y = quantity.lab, x = "Year", title = plot.title, color = "Export Source") +
    theme_bw() 
}
