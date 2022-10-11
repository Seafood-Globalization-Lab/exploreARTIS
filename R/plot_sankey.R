#' Creates a Sankey plot of ARTIS data
#' 
#' This is a function that creates a sankey plot showcasing seafood supply chains from producer to importer in the ARTIS dataset.
#' 
#' @param data an ARTIS dataframe.
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
#' @import ggsankey
#' @export
plot_sankey <- function(data, prop_flow_cutoff = 0.05, 
                        species = NA, years = NA,
                        producers = NA, exporters = NA, importers = NA,
                        hs_codes = NA, prod_method = NA, prod_environment = NA,
                        export_source = NA, 
                        weight = "live") {
  
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
  
  # Getting list of producers, exporters and importers--------------------------
  # based on proportional flow of trade summarized by those partners
  
  # Summarizing data based on quantity variable selected
  links <- data %>%
    group_by(source_country_iso3c, exporter_iso3c, importer_iso3c) %>%
    summarize(total_q = sum(.data[[quantity]], na.rm = TRUE))
  
  # Getting list of producers limited by proportional flow cutoff
  producers <- links %>%
    group_by(source_country_iso3c) %>%
    summarize(total_q = sum(total_q, na.rm=TRUE)) %>%
    ungroup() %>%
    mutate(total = sum(total_q, na.rm=TRUE)) %>%
    mutate(prop = total_q / total) %>%
    mutate(source_country_iso3c = case_when(
      prop < prop_flow_cutoff ~ "Other",
      TRUE ~ source_country_iso3c
    ))
  
  # Getting list of exporters limited by proportional flow cutoff
  exporters <- links %>%
    group_by(exporter_iso3c) %>%
    summarize(total_q = sum(total_q, na.rm=TRUE)) %>%
    ungroup() %>%
    mutate(total = sum(total_q, na.rm=TRUE)) %>%
    mutate(prop = total_q / total) %>%
    mutate(exporter_iso3c = case_when(
      prop < prop_flow_cutoff ~ "Other",
      TRUE ~ exporter_iso3c
    ))
  
  # Getting list of importers limited by proportional flow cutoff
  importers <- links %>%
    group_by(importer_iso3c) %>%
    summarize(total_q = sum(total_q, na.rm=TRUE)) %>%
    ungroup() %>%
    mutate(total = sum(total_q, na.rm=TRUE)) %>%
    mutate(prop = total_q / total) %>%
    mutate(importer_iso3c = case_when(
      prop < prop_flow_cutoff ~ "Other",
      TRUE ~ importer_iso3c
    ))
  
  # country names of producers, exporters, importers
  country_iso3c <- unique(
    c(producers$source_country_iso3c, 
      exporters$exporter_iso3c, 
      importers$importer_iso3c)
  )
  
  # Creating dataframe from sankey----------------------------------------------
  
  sankey_df <- links %>%
    # Filtering data based on prop flow cutoff
    filter(source_country_iso3c %in% country_iso3c,
           exporter_iso3c %in% country_iso3c,
           importer_iso3c %in% country_iso3c) %>%
    # Getting country names
    mutate(
      source_country_name = case_when(
        source_country_iso3c == "Other" ~ "Other",
        TRUE ~ countrycode(source_country_iso3c, origin = "iso3c", destination = "country.name")
      ),
      exporter_name = case_when(
        exporter_iso3c == "Other" ~ "Other",
        TRUE ~ countrycode(exporter_iso3c, origin = "iso3c", destination = "country.name")
      ),
      importer_name = case_when(
        importer_iso3c == "Other" ~ "Other",
        TRUE ~ countrycode(importer_iso3c, origin = "iso3c", destination = "country.name")
      )
    ) %>%
    # Tranforming into ggsankey format (x, node, next_x, next_node)
    ungroup() %>%
    make_long(source_country_name, exporter_name, importer_name, value = total_q)
  
  # Visualizing sankey diagram--------------------------------------------------
  sankey_df %>%
    ggplot(aes(x = x, 
               next_x = next_x, 
               node = node, 
               next_node = next_node,
               fill = factor(node),
               value = value,
               label = node)) +
    geom_sankey(flow.alpha = 0.6) +
    geom_sankey_label(size = 3, color = "white", fill = "gray40") +
    theme_sankey(base_size = 18) +
    labs(x = NULL) +
    theme(
      legend.position = "none",
      axis.text.x = element_blank()
    )
}