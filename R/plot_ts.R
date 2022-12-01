#' @import tidyverse
#' @import countrycode
#' @import viridis
#' @export
plot_ts <- function(data, artis_var = NA, trade_flow = NA, prop_flow_cutoff = 0.05,
                      species = NA, years = NA, producers = NA, 
                      exporters = NA, importers = NA, regions = NA,
                      hs_codes = NA, prod_method = NA, prod_environment = NA,
                      export_source = NA, region_self_loops = TRUE,
                      weight = "live",
                      plot.type = "line",
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
  } else if (artis_var == "exporter_iso3c") {
    color.lab <- "Exporters"
  } else if (artis_var == "importer_iso3c") {
    color.lab <- "Importers"
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
  
  
  # Special case for summarizing by region for trading partners
  if (!is.na(regions)) {
    
    if (regions == "owid") {
      # Defaul regional definitions - Our World in Data
      data <- convert_owid_regions(data)
      
    } else {
      data <- data %>%
        # Initial conversion from iso3c codes to regions
        mutate(exporter_region = suppressWarnings(countrycode(exporter_iso3c, origin = "iso3c", destination = regions)),
               importer_region = suppressWarnings(countrycode(importer_iso3c, origin = "iso3c", destination = regions))) %>%
        # Cleaning up any NAs (ie Other nei)
        mutate(
          exporter_region = case_when(
            is.na(exporter_region) ~ "Other",
            TRUE ~ exporter_region),
          importer_region = case_when(
            is.na(importer_region) ~ "Other",
            TRUE ~ importer_region
          )
        )
    }
    
    # Check if self loops need to be removed
    if (region_self_loops == FALSE) {
      data <- data %>%
        filter(exporter_region != importer_region)
    }
    
    # variable set up for summary below
    if (artis_var == "exporter_iso3c") {
      data <- data %>%
        rename("exporter_iso3c" = "exporter_region")
    } else {
      data <- data %>%
        rename("importer_iso3c" = "importer_region")
    }
  }
  
  
  # Getting timeseries of data by variable selected
  data <- data %>%
    group_by(year, .data[[artis_var]]) %>%
    summarize(quantity = sum(.data[[quantity]], na.rm = TRUE)) %>%
    ungroup()
  
  colnames(data) <- c("year", "variable", "quantity")
  
  # Checking against prop_flow cutoff if necessary
  if (!is.na(prop_flow_cutoff)) {
    data <- data %>%
      group_by(year) %>%
      mutate(annual = sum(quantity, na.rm = TRUE)) %>%
      ungroup() %>%
      mutate(prop = quantity / annual) %>%
      mutate(variable = case_when(
        prop < prop_flow_cutoff ~ "Other",
        TRUE ~ variable
      )) %>%
      # Re-summarize based on some categories turned to "Other"
      group_by(year, variable) %>%
      summarize(quantity = sum(quantity, na.rm = TRUE)) %>%
      ungroup()
  }
  
  # Filling in missing values for any years with zeros
  year_grid <- expand_grid(year = unique(data$year), 
                           variable = unique(data$variable))
  
  data <- data %>%
    full_join(year_grid,
              by = c("year", "variable")) %>%
    mutate(quantity = if_else(is.na(quantity), true = 0, false = quantity))
  
  if (artis_var == "sciname") {
    # Format scinames for presentation
    data <- data %>%
      mutate(variable = str_to_sentence(variable))
  }
  
  if ((artis_var == "exporter_iso3c" | artis_var == "importer_iso3c") & is.na(regions)) {
    # Rename ISO 3 codes to country names
    data <- data %>%
      left_join(
        owid_regions %>%
          select(code, country_name),
        by = c("variable" = "code")
      ) %>%
      mutate(country_name = case_when(
        is.na(country_name) ~ "Other",
        TRUE ~ country_name
      )) %>%
      # Regroup if there are multiple "Other" countries
      group_by(year, country_name) %>%
      summarize(quantity = sum(quantity, na.rm = TRUE)) %>%
      ungroup() %>%
      rename(variable = country_name)
  }
  
  # Reorder (descending) based on quantity
  data <- data %>%
    mutate(variable = fct_reorder(variable, quantity))
  
  # Reorder so that "Other" always last
  if ("Other" %in% unique(data$variable)) {
    data <- data %>%
      mutate(variable = forcats::fct_relevel(variable, "Other", after = Inf))
  }
  
  #-----------------------------------------------------------------------------
  # Visualizing timeseries as line graphs
  
  if (plot.type == "line") {
    
    data %>%
      ggplot(aes(x = year, y = quantity, color = variable)) +
      geom_line(size = 1.1) +
      scale_color_manual(values = artis_palette(length(unique(data$variable)))) +
      labs(y = quantity.lab, x = "Year", title = plot.title, color = color.lab) +
      theme_bw()
  } else {
    data %>%
      ggplot() +
      geom_area(aes(x = year, y = quantity, fill = variable)) +
      scale_fill_manual(values = artis_palette(length(unique(data$variable)))) +
      labs(y = quantity.lab, x = "Year", title = plot.title, fill = color.lab) +
      theme_bw() 
  }
  
}
