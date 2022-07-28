#' @import tidyverse
#' @import countrycode
#' @import viridis
#' @export

plot_partner_stacked <- function(data, trade_flow = "export", prop_flow_cutoff = 0.05, 
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
    
  # trade_flow = import to plot import partners from the focal region
  # trade_flow = export to plot export partners to the focal region
    if(trade_flow == "import"){
      partner <- "importer_iso3c"
      partner.lab <- "Importer"
    }else{
      partner <- "exporter_iso3c"
      partner.lab <- "Exporter"
    }
    
  data <- data %>%
      group_by(year, .data[[partner]]) %>%
      summarise(quantity = sum(.data[[quantity]], na.rm = TRUE)) %>%
      # Use percentage of total quantity as the cutoff:
      group_by(year) %>%
      mutate(global_annual = sum(quantity)) %>%
      mutate(prop_flow = quantity / global_annual) %>%
      mutate(partner = if_else(prop_flow < prop_flow_cutoff, true = "Other", false = .data[[partner]])) %>%
      group_by(year, partner) %>%
      summarise(quantity = sum(quantity)) %>%
      ungroup()
    
    # Create full list of partners and years 
    partner_year_grid <- expand_grid(partner = unique(data$partner), 
                                     year = unique(data$year))
    
    
    # Bind stacked line graph data to grid so that zeroes can be filled in for "missing" years
    # Need to do this for time series to plot correctly
    data %>%
      full_join(partner_year_grid, by = c("year", "partner")) %>%
      mutate(quantity = if_else(is.na(quantity), true = 0, false = quantity)) %>%
      mutate(partner.name = suppressWarnings(countrycode(partner, origin = "iso3c", destination = "country.name"))) %>%
      mutate(partner.name = ifelse(is.na(partner.name), "Other", partner.name)) %>%
      mutate(partner.name = fct_reorder(partner.name, quantity)) %>%
      ggplot() +
      geom_area(aes(x = year, y = quantity, fill = partner.name)) +
      scale_fill_viridis_d() +
      labs(y = quantity.lab, x = "Year", title = plot.title, fill = partner.lab) +
      theme_bw() 
}
