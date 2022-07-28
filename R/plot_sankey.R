#' @import tidyverse
#' @import countrycode
#' @import networkD3
#' @export

plot_sankey <- function(data, prop_flow_cutoff = 0.05, 
                                           species = NA, years = NA,
                                           producers = NA, exporters = NA, importers = NA,
                                           hs_codes = NA, prod_method = NA, prod_environment = NA,
                                           export_source = NA, 
                                           weight = "live") {
  
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
  
  # Links dataframe
  links <- data %>%
    mutate(exporter_name = suppressWarnings(countrycode(exporter_iso3c, origin = "iso3c", destination = "country.name")),
           importer_name = suppressWarnings(countrycode(importer_iso3c, origin = "iso3c", destination = "country.name")),
           source_country_name = suppressWarnings(countrycode(source_country_iso3c, origin = "iso3c", destination = "country.name"))) %>%
    group_by(exporter_name, importer_name, source_country_name) %>%
    summarize(total_q = sum(.data[[quantity]], na.rm=TRUE)) %>%
    filter(!is.na(source_country_name) & !is.na(exporter_name) & !is.na(importer_name))
  
  # Getting list of producers limited by proportional flow cutoff
  producers <- links %>%
    group_by(source_country_name) %>%
    summarize(total_q = sum(total_q, na.rm=TRUE)) %>%
    ungroup() %>%
    mutate(total = sum(total_q, na.rm=TRUE)) %>%
    mutate(prop = total_q / total) %>%
    mutate(source_country_name = case_when(
      prop < prop_flow_cutoff ~ "Other",
      TRUE ~ source_country_name
    ))
  
  # Getting list of exporters limited by proportional flow cutoff
  exporters <- links %>%
    group_by(exporter_name) %>%
    summarize(total_q = sum(total_q, na.rm=TRUE)) %>%
    ungroup() %>%
    mutate(total = sum(total_q, na.rm=TRUE)) %>%
    mutate(prop = total_q / total) %>%
    mutate(exporter_name = case_when(
      prop < prop_flow_cutoff ~ "Other",
      TRUE ~ exporter_name
    ))
  
  # Getting list of importers limited by proportional flow cutoff
  importers <- links %>%
    group_by(importer_name) %>%
    summarize(total_q = sum(total_q, na.rm=TRUE)) %>%
    ungroup() %>%
    mutate(total = sum(total_q, na.rm=TRUE)) %>%
    mutate(prop = total_q / total) %>%
    mutate(importer_name = case_when(
      prop < prop_flow_cutoff ~ "Other",
      TRUE ~ importer_name
    ))
  
  # country names of producers, exporters, importers
  country_names <- unique(c(producers$source_country_name, exporters$exporter_name, importers$importer_name))
  
  # producers exporter trade flows
  producer_exporter <- links %>%
    mutate(source_country_name = case_when(
      !(source_country_name %in% country_names) ~ "Other",
      TRUE ~ source_country_name
    ),
    exporter_name = case_when(
      !(exporter_name %in% country_names) ~ "Other",
      TRUE ~ exporter_name
    )) %>%
    group_by(source_country_name, exporter_name) %>%
    summarize(total_q = sum(total_q, na.rm=TRUE)) %>%
    mutate(source_country_name = paste(source_country_name, "_source", sep=""),
           exporter_name = paste(exporter_name, "_exp", sep="")) %>%
    rename(source = source_country_name,
           target = exporter_name)
  
  # exporters importers trade flows
  exporter_importer <- links %>%
    mutate(exporter_name = case_when(
      !(exporter_name %in% country_names) ~ "Other",
      TRUE ~ exporter_name
    ),
    importer_name = case_when(
      !(importer_name %in% country_names) ~ "Other",
      TRUE ~ importer_name
    )) %>%
    group_by(exporter_name, importer_name) %>%
    summarize(total_q = sum(total_q, na.rm=TRUE)) %>%
    mutate(exporter_name = paste(exporter_name, "_exp", sep=""),
           importer_name = paste(importer_name, "_imp", sep="")) %>%
    rename(source = exporter_name,
           target = importer_name)
  
  # Joining producer exporter and exporter importer trade flows together
  trade_flows <- producer_exporter %>%
    bind_rows(exporter_importer)
  
  # Creating nodes dataframe
  nodes <- data.frame(
    name = unique(c(trade_flows$source, trade_flows$target))
  ) %>% 
    mutate(label = gsub("_.{3,6}", "", name))
  
  # Mapping source and target names to respective row in the nodes dataframe, 0-indexed
  trade_flows <- trade_flows %>%
    mutate(source_id = match(source, nodes$name) - 1,
           target_id = match(target, nodes$name) - 1)
  
  sankeyNetwork(Links = trade_flows, Nodes = nodes, Source = "source_id", Target = "target_id",
                Value = "total_q", NodeID = "label")
}

