#' ARTIS Bar Chart
#' 
#' Function that creates a bar chart for ARTIS data.
#' 
#' @param data an ARTIS dataframe.
#' @param bar_group refers to which column for the bars that will be visualized (ie exporter_iso3c, importer_iso3c, sciname)
#' @param species list of species/species groups to include, default NA - includes all species.
#' @param years list of years to include, default NA - includes all years.
#' @param producers list of producers (as iso3 codes) to include, default NA - includes all producers.
#' @param exporters list of exporters (as iso3 codes) to include, default NA - includes all exporters.
#' @param importers list of importers (as iso3 codes) to include, default NA - includes all importers.
#' @param hs_codes list of hs level 6 codes to include, default NA - includes all hs6 codes.
#' @param prod_method list of production methods (capture, aquaculture, or unknown), default NA - includes all production methods.
#' @param prod_environment list of environments (marine, inland, or unknown), default NA - includes all environments
#' @param export_source list of types of export (domestic export, foreign export, or error export), default NA - all export sources.
#' @param fill_type column to use to stack the bars (ie by method, habitat)
#' @param weight trade quantity type to visualize ("live" for live weight or "product" for product weight), default "live."
#' @import tidyverse
#' @import countrycode
#' @import viridis
#' @export

plot_bar <- function(data, bar_group, species = NA, years = NA,
                     producers = NA, exporters = NA, importers = NA,
                     hs_codes = NA, prod_method = NA, prod_environment = NA,
                     export_source = NA, regions = NA, weight = "live_weight_t",
                     common_names = FALSE, fill_type = NA, top_n = 10, 
                     y.lab = "", x.lab = "quantity", fill.lab = "",
                     plot.title = "", facet_variable = NA, facet_n = NA){
  
  if (is.na(bar_group)) {
    warning("please select a valid bar group to plot.")
    return(NULL)
  } else if (!is.na(facet_variable)) {
    
    if (bar_group == facet_variable) {
      warning("bar group and facet variable cannot be the same.")
      return(NULL)
    }
  }
  
  # data should be an ARTIS data frame
  # of the total trade are lumped together as "Other"
  
  # Setting up parameters based on user input-----------------------------------
  # Select live or product weight
  # Select live or product weight
  quantity <- weight
  # If no quantity (y-axis) label is provided try to provide a default option
  if (is.na(x.lab)) {
    if(quantity == "live_weight_t"){
      x.lab <- "Quantity (t live weight)"
    } else if (quantity == "product_weight_t") {
      x.lab <- "Quantity (t product weight)"
    } else {
      x.lab <- ""
    }
  }
  
  if (!is.na(fill_type)) {
    if (fill_type == "dom_source") {
      fill.lab <- "Export Source"
    } else if (fill_type == "method") {
      fill.lab <- "Production Method"
    } 
  }
  
  # Filter to data selection based on user input--------------------------------
  data <- filter_artis(data, species, years, producers, exporters, importers,
                       hs_codes, prod_method, prod_environment, export_source)
  
  
  data <- data %>%
    rename("bar_group" = .data[[bar_group]])
  
  # Adding country names or regional names
  if (bar_group == "exporter_iso3c" | bar_group == "importer_iso3c") {
    if (is.na(regions)) {
      data <- data %>%
        left_join(
          owid_regions %>%
            select(code, country_name),
          by = c("bar_group" = "code")
        ) %>%
        mutate(bar_group = country_name) %>%
        select(-country_name)
      
    } else {
      data <- data %>%
        left_join(
          owid_regions %>%
            select(code, region),
          by = c("bar_group" = "code")
        ) %>%
        mutate(region = case_when(
          bar_group == "NEI" ~ "Other",
          TRUE ~ region
        )) %>%
        mutate(bar_group = region) %>%
        select(-region)
    }
  }
  
  # Formatting and name cleaning for scinames
  if (bar_group == "sciname") {
    
    if (common_names == TRUE) {
      data <- data %>%
        left_join(sciname_metadata %>%
                    select(sciname, common_name),
                  by = c("bar_group" = "sciname")) %>%
        mutate(bar_group = common_name) %>%
        select(-common_name)
    }
    
    # Format scinames for presentation
    data <- data %>%
      mutate(bar_group = str_to_sentence(bar_group))
  }
  
  
  # Factors for bar ordering----------------------------------------------------
  if("dom_source" %in% colnames(data)){
    data$dom_source <- factor(data$dom_source,
                              levels = c("domestic", "foreign", "error"))
  }

  if("method" %in% colnames(data)){
    data$method <- factor(data$method,
                          levels = c("aquaculture", "capture", "unknown"))
  }
  
  # Summarizing data by bar group, fill type and facetting group
  grouping_cols <- c("bar_group")
  
  if (!is.na(fill_type)) {
    grouping_cols <- c(grouping_cols, fill_type)
  }
  
  if (!is.na(facet_variable)) {
    grouping_cols <- c(grouping_cols, facet_variable)
  }
  
  data <- data %>%
    group_by(across(grouping_cols)) %>%
    summarise(quantity = sum(.data[[quantity]], na.rm=TRUE)) %>%
    ungroup()
  
  if (!is.na(fill_type) & is.na(facet_variable)) {
    colnames(data) <- c("bar_group", "fill_type", "quantity")
  } else if (!is.na(fill_type) & !is.na(facet_variable)) {
    colnames(data) <- c("bar_group", "fill_type", "facet_variable", "quantity")
  } else if (is.na(fill_type) & !is.na(facet_variable)) {
    colnames(data) <- c("bar_group", "facet_variable", "quantity")
  }
  
  # facetting
  if (!is.na(facet_variable)) {
    data <- data %>%
      group_by(facet_variable) %>%
      slice_max(order_by = quantity, n = top_n) %>%
      ungroup()# %>%
      # mutate(bar_group = reorder_within(bar_group, quantity, facet_variable))
    
    top_facet_n <- data %>%
      group_by(facet_variable) %>%
      summarize(facet_total = sum(quantity, na.rm = TRUE)) %>%
      ungroup() %>%
      slice_max(order_by = facet_total, n = facet_n)
    
    data <- data %>%
      filter(facet_variable %in% unique(top_facet_n$facet_variable)) %>%
      # only group by bar group and facet variable since fill type may still be selected
      group_by(bar_group, facet_variable) %>%
      mutate(facet_total = sum(quantity, na.rm = TRUE)) %>%
      ungroup() %>%
      mutate(bar_group = reorder_within(bar_group, facet_total, facet_variable))
    
  } else if (is.na(facet_variable)) {
    # Summarizing for top n variables
    data <- data %>%
      group_by(bar_group) %>%
      mutate(total = sum(quantity, na.rm = TRUE)) %>%
      ungroup()
    
    # Top N bars in the bar chart
    top_n_value <- data %>%
      select(bar_group, total) %>%
      distinct() %>%
      slice_max(order_by = total, n = top_n) %>%
      pull(total) %>%
      min()
    
    data <- data %>%
      filter(total >= top_n_value) %>%
      mutate(bar_group = reorder(bar_group, total))
  }
  
  # Bar Chart Creation----------------------------------------------------------
  
  p <- ggplot(data, aes(x = quantity, y = bar_group))
  
  if (!is.na(fill_type)) {
    p <- p +
      geom_bar(stat = "identity", aes(fill = fill_type)) +
      scale_fill_manual(values = artis_palette(length(unique(data$fill_type))))
  } else {
    p <- p +
      geom_bar(stat = "identity", fill = "#114F59")
  }
  
  if (!is.na(facet_variable)) {
    p <- p +
      facet_wrap(as.formula(paste(".~", "facet_variable")), scales = "free") +
      scale_y_reordered()
  }
  
  p <- p +
    labs(y = y.lab, x = x.lab, fill = fill.lab) +
    theme_bw()

  return(p)
}
