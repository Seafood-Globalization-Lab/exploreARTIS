#' @import tidyverse
#' @import countrycode
#' @import viridis
#' @export
plot_ts <- function(data, artis_var = NA, trade_flow = NA, prop_flow_cutoff = 0.05,
                      species = NA, years = NA, producers = NA, 
                      exporters = NA, importers = NA, regions = NA,
                      hs_codes = NA, prod_method = NA, prod_environment = NA,
                      export_source = NA, region_self_loops = TRUE,
                      weight = "live", facet_variable = NA, facet_values = NA,
                      plot.type = "line",
                      plot.title = "") {
  
  if(is.na(artis_var)) {
    warning("please select a variable to plot")
    return(NULL)
  }
  
  if (!is.na(facet_variable)) {
    if (artis_var == facet_variable) {
      warning("artis_var cannot be the same as facet_variable")
      return(NULL)
    }
  }
  
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
  data <- filter_artis(data, species, years, producers, exporters, importers,
                       hs_codes, prod_method, prod_environment, export_source)
  
  
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
  
  grouping_cols <- c("year", artis_var)
  
  if (!is.na(facet_variable)) {
    grouping_cols <- c(grouping_cols, facet_variable)
  }
  
  
  # Getting timeseries of data by variable selected
  data <- data %>%
    # group_by(year, .data[[artis_var]]) %>%
    group_by(across(grouping_cols)) %>%
    summarize(quantity = sum(.data[[quantity]], na.rm = TRUE)) %>%
    ungroup()
  
  if (!is.na(facet_variable)) {
    colnames(data) <- c("year", "variable", "facet_var", "quantity")
  } else {
    colnames(data) <- c("year", "variable", "quantity")
  }
  
  # limiting facets by values provided
  if (!is.na(facet_variable)) {
    if (typeof(facet_values) == "character") {
      data <- data %>%
        filter(facet_var %in% facet_values)
    } else if (typeof(facet_values) == "double") {
      data <- data %>%
        group_by(facet_var) %>%
        mutate(total = sum(quantity, na.rm = TRUE)) %>%
        ungroup()
      
      top_n_value <- data %>%
        select(total) %>%
        distinct() %>%
        arrange(desc(total)) %>%
        slice_max(order_by = total, n = facet_values) %>%
        pull(total) %>%
        min()
      
      data <- data %>%
        filter(total >= top_n_value) 
      
    } else {
      warning("entered invalid facet values")
      return(NULL)
    }
  }
  
  # Checking against prop_flow cutoff if necessary
  prop_flow_cols <- c("year")
  if(!is.na(facet_variable)) {
    prop_flow_cols <- c(prop_flow_cols, "facet_var")
  }
  
  if (!is.na(prop_flow_cutoff)) {
    data <- data %>%
      group_by(across(prop_flow_cols)) %>%
      mutate(annual = sum(quantity, na.rm = TRUE)) %>%
      ungroup() %>%
      mutate(prop = quantity / annual) %>%
      mutate(variable = case_when(
        prop < prop_flow_cutoff ~ "Other",
        TRUE ~ variable
      ))
    
    prop_flow_cols <- unique(c(prop_flow_cols, "variable"))
    
    data <- data %>%
      # Re-summarize based on some categories turned to "Other"
      group_by(across(prop_flow_cols)) %>%
      summarize(quantity = sum(quantity, na.rm = TRUE)) %>%
      ungroup()
  }
  
  
  
  # Filling in missing values for any years with zeros
  df_variables <- c()
  if (!is.na(facet_variable)) {
    df_variables <- c("year", "variable", "facet_var")
    year_grid <- expand_grid(year = unique(data$year), 
                             variable = unique(data$variable),
                             facet_var = unique(data$facet_var))
  } else {
    df_variables <- c("year", "variable")
    year_grid <- expand_grid(year = unique(data$year), 
                             variable = unique(data$variable))
  }
  
  data <- data %>%
    full_join(year_grid,
              by = df_variables) %>%
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
  if (!is.na(facet_variable)) {
    data <- data %>%
      group_by(year, facet_var) %>%
      mutate(variable = fct_reorder(variable, quantity)) %>%
      ungroup()
  } else {
    data <- data %>%
      group_by(year) %>%
      mutate(variable = fct_reorder(variable, quantity)) %>%
      ungroup()
  }
  
  
  # Reorder so that "Other" always last
  if ("Other" %in% unique(data$variable)) {
    data <- data %>%
      mutate(variable = forcats::fct_relevel(variable, "Other", after = Inf))
  }
  
  #-----------------------------------------------------------------------------
  # Visualizing timeseries as line graphs
  
  if (plot.type == "line") {
    
    p <- data %>%
      ggplot(aes(x = year, y = quantity, color = variable)) +
      geom_line(size = 1.1) +
      scale_color_manual(values = artis_palette(length(unique(data$variable)))) +
      labs(y = quantity.lab, x = "Year", title = plot.title, color = color.lab) +
      theme_bw()
  } else {
    p <- data %>%
      ggplot() +
      geom_area(aes(x = year, y = quantity, fill = variable)) +
      scale_fill_manual(values = artis_palette(length(unique(data$variable)))) +
      labs(y = quantity.lab, x = "Year", title = plot.title, fill = color.lab) +
      theme_bw() 
  }
  
  if (!is.na(facet_variable)) {
    p <- p + 
      facet_wrap(as.formula(paste(".~", "facet_var")))
  }
  
  return(p)
}
