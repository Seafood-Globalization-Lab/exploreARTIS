#' @import tidyverse
#' @import countrycode
#' @import viridis
#' @export
plot_line <- function(data, artis_var = NA, trade_flow = NA, prop_flow_cutoff = 0.05, 
                      species = NA, years = NA, producers = NA, 
                      exporters = NA, importers = NA, regions = NA,
                      hs_codes = NA, prod_method = NA, prod_environment = NA,
                      export_source = NA, region_self_loops = TRUE,
                      weight = "live",
                      plot.title = "") {
  
  #-----------------------------------------------------------------------------
  # Initial variable setup
  
  # Select live or product weight
  if(weight == "live"){
    quantity <- "live_weight_t"
    quantity.lab <- "Quantity (t live weight)"
  } else {
    quantity <- "product_weight_t"
    quantity.lab <- "Quantity (t product weight)"
  }
  
  if (artis_var == "partner") {
    # trade_flow = import to plot import partners from the focal region
    # trade_flow = export to plot export partners to the focal region
    if (trade_flow == "import") {
      partner <- "importer_iso3c"
      partner.lab <- "Importer"
    } else {
      partner <- "exporter_iso3c"
      partner.lab <- "Exporter"
    }
  }
  
  # Labels for Legend
  if (artis_var == "method") {
    color.lab <- "Production Method"
  } else if (artis_var == "dom_source") {
    color.lab <- "Export Source"
  } else if (artis_var == "hs6") {
    color.lab <- "HS Product"
  } else {
    color.lab <- "Species / Species groups"
  }
  
  #-----------------------------------------------------------------------------
  # Filtering data based on user input
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
  
  
  #-----------------------------------------------------------------------------
  # Summarize data based on variable selected
  
  # Special case for summarizing by country and region
  if (artis_var == "partner") {
    print("in partner data summarizing")
    
  } else {
    # Getting timeseries of data by variable selected
    data <- data %>%
      group_by(year, .data[[artis_var]]) %>%
      summarize(quantity = sum(.data[[quantity]], na.rm = TRUE)) %>%
      ungroup()
    
    colnames(data) <- c("year", "variable", "quantity")
    
    # Filling in missing values for any years with zeros
    year_grid <- expand_grid(year = unique(data$year), 
                             variable = unique(data$variable))
    
    data <- data %>%
      full_join(year_grid,
                by = c("year", "variable")) %>%
      mutate(quantity = if_else(is.na(quantity), true = 0, false = quantity))
  }
  
  
  #-----------------------------------------------------------------------------
  # Visualizing timeseries as line graphs
  
  data %>%
    ggplot(aes(x = year, y = quantity, color = variable)) +
    geom_line(size = 1.1) +
    scale_color_manual(values = artis_palette(length(unique(data$variable)))) +
    labs(y = quantity.lab, x = "Year", title = plot.title, color = color.lab) +
    theme_bw()
}
