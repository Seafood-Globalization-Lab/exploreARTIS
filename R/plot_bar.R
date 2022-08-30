#' @import tidyverse
#' @import countrycode
#' @import viridis
#' @export

plot_bar <- function(data, bar_group, species = NA, years = NA,
                             producers = NA, exporters = NA, importers = NA,
                             hs_codes = NA, prod_method = NA, prod_environment = NA,
                             export_source = NA, 
                             weight = "live", 
                             fill_type = NA, top_n = 10, 
                             plot.title = ""){
  # data should be an ARTIS data frame
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
  
  if(is.na(fill_type) == TRUE){
    data %>% 
      group_by(.data[[bar_group]]) %>%
      summarise(quantity = sum(.data[[quantity]], na.rm=TRUE)) %>%
      ungroup() %>%
      slice_max(order_by = quantity, n = top_n) %>%
      rename("bar_group" = .data[[bar_group]]) %>%
      mutate(bar_group = fct_reorder(bar_group, quantity)) %>%
      ggplot(aes(x = quantity, y = bar_group)) +
      geom_bar(stat = "identity") +
      labs(x = quantity.lab, y = "") +
      theme_bw()
  }else{
    top_n_list <- data %>%
      rename("bar_group" = .data[[bar_group]]) %>%
      group_by(bar_group) %>%
      summarise(total_quantity = sum(.data[[quantity]], na.rm=TRUE)) %>%
      ungroup() %>%
      slice_max(order_by = total_quantity, n = top_n)
    
    top_n_list %>% 
      left_join(data %>% 
                  rename("bar_group" = .data[[bar_group]]), 
                by = "bar_group") %>% 
      mutate(bar_group = fct_reorder(bar_group, total_quantity)) %>%
      group_by(bar_group, .data[[fill_type]]) %>%
      summarise(quantity = sum(.data[[quantity]], na.rm=TRUE)) %>%
      ggplot(aes(x = quantity, y = bar_group, fill = .data[[fill_type]])) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = c("#F7AF75", "#2E8D9A", "#86ADA7")) +
      labs(x = quantity.lab, y = "", title = plot.title) +
      theme_bw()
  }
}
