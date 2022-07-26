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
                     export_source = NA,
                     regions = NA,
                     weight = "live",
                     common_names = FALSE,
                     fill_type = NA, top_n = 10, 
                     plot.title = ""){
  # data should be an ARTIS data frame
  # of the total trade are lumped together as "Other"
  
  # Setting up parameters based on user input-----------------------------------
  # Select live or product weight
  if (weight == "live") {
    quantity <- "live_weight_t"
    quantity.lab <- "Quantity (t live weight)"
  } else {
    quantity <- "product_weight_t"
    quantity.lab <- "Quantity (t product weight)"
  }
  
  if (!is.na(fill_type)) {
    if (fill_type == "dom_source") {
      fill_lab <- "Export Source"
    } else if (fill_type == "method") {
      fill_lab <- "Production Method"
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
  data$dom_source <- factor(data$dom_source,
                            levels = c("domestic export", "foreign export", "error export"))
  
  data$method <- factor(data$method,
                        levels = c("aquaculture", "capture", "unknown"))
  
  # Bar Chart Creation----------------------------------------------------------
  
  if(is.na(fill_type) == TRUE){
    # Bar chart with no stacking "fill" group
    data %>% 
      # Summarizing data based on column for bars "bar group"
      group_by(bar_group) %>%
      summarise(quantity = sum(.data[[quantity]], na.rm=TRUE)) %>%
      ungroup() %>%
      # Getting top n values
      slice_max(order_by = quantity, n = top_n) %>%
      mutate(bar_group = fct_reorder(bar_group, quantity)) %>%
      # Creating Bar Chart
      ggplot(aes(x = quantity, y = bar_group)) +
      geom_bar(stat = "identity", fill = "#114F59") +
      labs(x = quantity.lab, y = "") +
      theme_bw()
  } else {
    # Case when there is a "fill" or stacking column for bar chart
    
    # Getting a list of top n bars
    top_n_list <- data %>%
      group_by(bar_group) %>%
      summarise(total_quantity = sum(.data[[quantity]], na.rm=TRUE)) %>%
      ungroup() %>%
      slice_max(order_by = total_quantity, n = top_n)
    
    # Getting data for the top n bars
    top_n_list %>% 
      left_join(data, 
                by = "bar_group") %>% 
      mutate(bar_group = fct_reorder(bar_group, total_quantity)) %>%
      group_by(bar_group, .data[[fill_type]]) %>%
      summarise(quantity = sum(.data[[quantity]], na.rm=TRUE)) %>%
      # Creating Bar Chart
      ggplot(aes(x = quantity, y = bar_group, fill = .data[[fill_type]])) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = c("#114F59", "#741A32", "#D38F35")) +
      labs(x = quantity.lab, y = "", fill = fill_lab, title = plot.title) +
      theme_bw()
  }
}
