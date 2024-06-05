#' Creates a stacked graph import or export partners from ARTIS data
#' 
#' This is a function that creates a stacked graph of traded species with one line per species.
#' 
#' @param data an ARTIS dataframe.
#' @param trade_flow select whether to plot export partners ("export") or import partners ("import") - default is "export."
#' @param prop_flow_cutoff default prop_flow_cutoff = 0.05 means trade volumes that comprise less than 5\% of the total trade are lumped together as "Other".
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

plot_hs_product_stacked <- function(data, trade_flow = "export", prop_flow_cutoff = 0.05, 
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
  } else {
    quantity <- "product_weight_t"
    quantity.lab <- "Quantity (t product weight)"
  }
  
  # trade_flow = import to plot import partners from the focal region
  # trade_flow = export to plot export partners to the focal region
  if(trade_flow == "import"){
    partner <- "importer_iso3c"
    partner.lab <- "Importer"
  }else{
    partner <- "exporter_iso3c"
    partner.lab <- "Exporter"
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
  
  # Creating dataframe hs code by year------------------------------------------
  data <- data %>%
    group_by(year, hs6) %>%
    summarise(quantity = sum(.data[[quantity]], na.rm = TRUE)) %>%
    # Use percentage of total quantity as the cutoff:
    group_by(year) %>%
    mutate(global_annual = sum(quantity)) %>%
    mutate(prop_flow = quantity / global_annual) %>%
    # Renaming based on prop flow cutoff
    mutate(hs6 = if_else(prop_flow < prop_flow_cutoff, true = "Other", false = hs6)) %>%
    # Resummarize based on new naming
    group_by(year, hs6) %>%
    summarise(quantity = sum(quantity)) %>%
    ungroup()
  
  # Create full list of partners and years 
  hs6_year_grid <- expand_grid(year = unique(data$year),
                               hs6 = unique(data$hs6))
  
  
  # Bind stacked line graph data to grid so that zeroes can be filled in for "missing" years
  # Need to do this for time series to plot correctly
  data %>%
    full_join(hs6_year_grid, by = c("year", "hs6")) %>%
    mutate(quantity = if_else(is.na(quantity), true = 0, false = quantity)) %>%
    mutate(hs6 = fct_reorder(hs6, quantity)) %>%
    # Reorder so that "Other" always last
    mutate(hs6 = forcats::fct_relevel(hs6, "Other", after = Inf)) %>%
    ungroup() %>%
    group_by(year, hs6) %>%
    summarize(quantity = sum(quantity)) %>%
    ungroup() %>%
    # Plot stacked line graph
    ggplot() +
    geom_area(aes(x = year, y = quantity, fill = hs6)) +
    scale_fill_manual(values = artis_palette(length(unique(data$hs6)))) +
    labs(y = quantity.lab, x = "Year", title = plot.title, fill = "HS 6 Digit Code") +
    theme_bw() 
}
