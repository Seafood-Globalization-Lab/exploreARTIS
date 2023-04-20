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
plot_sankey <- function(data, prop_flow_cutoff = 0.05, regions = NA,
                        species = NA, years = NA,
                        producers = NA, exporters = NA, importers = NA,
                        hs_codes = NA, prod_method = NA, prod_environment = NA,
                        export_source = NA, 
                        weight = "live") {
  
  # Setting up parameters based on user input-----------------------------------
  
  # Select live or product weight
  if (weight == "live") {
    quantity <- "live_weight_t"
    quantity.lab <- "Quantity (t live weight)"
  } else {
    quantity <- "product_weight_t"
    quantity.lab <- "Quantity (t product weight)"
  }
  
  # Filtering data based on user input------------------------------------------
  data <- filter_artis(data, species, years, producers, exporters, importers,
                       hs_codes, prod_method, prod_environment, export_source)
  
  # Getting list of producers, exporters and importers--------------------------
  # based on proportional flow of trade summarized by those partners
  
  # Summarizing data based on quantity variable selected
  links <- data %>%
    group_by(source_country_iso3c, exporter_iso3c, importer_iso3c) %>%
    summarize(total_q = sum(.data[[quantity]], na.rm = TRUE))
  
  # Summarizing by region if requested
  if (!is.na(regions)) {
    
    if (regions == "owid") {
      links <- links %>%
        # producer regions
        left_join(
          owid_regions %>%
            select(code, source_region = region),
          by = c("source_country_iso3c" = "code")
        ) %>%
        # exporter regions
        left_join(
          owid_regions %>%
            select(code, exporter_region = region),
          by = c("exporter_iso3c" = "code")
        ) %>%
        # importer regions
        left_join(
          owid_regions %>%
            select(code, importer_region = region),
          by = c("importer_iso3c" = "code")
        )
    } else {
      
      links <- links %>%
        mutate(
          source_region = suppressWarnings(countrycode(source_country_iso3c, origin = "iso3c", destination = regions)),
          exporter_region = suppressWarnings(countrycode(exporter_iso3c, origin = "iso3c", destination = regions)),
          importer_region = suppressWarnings(countrycode(importer_iso3c, origin = "iso3c", destination = regions))
        )
    }
    
    # Cleaning and re-summarizing regions
    links <- links %>%
      # Cleaning any codes that do not get a region
      mutate(source_region = case_when(
        is.na(source_region) ~ "Other",
        TRUE ~ source_region
      )) %>%
      mutate(exporter_region = case_when(
        is.na(exporter_region) ~ "Other",
        TRUE ~ exporter_region
      )) %>%
      mutate(importer_region = case_when(
        is.na(importer_region) ~ "Other",
        TRUE ~ importer_region
      )) %>%
      group_by(source_region, exporter_region, importer_region) %>%
      summarize(total_q = sum(total_q, na.rm = TRUE)) %>%
      ungroup()
  }
  
  # Renaming for consistency
  colnames(links) <- c("producer", "exporter", "importer", "total_q")

  
  # Getting list of producers limited by proportional flow cutoff
  producers <- links %>%
    group_by(producer) %>%
    summarize(total_q = sum(total_q, na.rm=TRUE)) %>%
    ungroup() %>%
    mutate(total = sum(total_q, na.rm=TRUE)) %>%
    mutate(prop = total_q / total) %>%
    mutate(producer = case_when(
      prop < prop_flow_cutoff ~ "Other",
      TRUE ~ producer
    ))
  
  # Getting list of exporters limited by proportional flow cutoff
  exporters <- links %>%
    group_by(exporter) %>%
    summarize(total_q = sum(total_q, na.rm=TRUE)) %>%
    ungroup() %>%
    mutate(total = sum(total_q, na.rm=TRUE)) %>%
    mutate(prop = total_q / total) %>%
    mutate(exporter = case_when(
      prop < prop_flow_cutoff ~ "Other",
      TRUE ~ exporter
    ))
  
  # Getting list of importers limited by proportional flow cutoff
  importers <- links %>%
    group_by(importer) %>%
    summarize(total_q = sum(total_q, na.rm=TRUE)) %>%
    ungroup() %>%
    mutate(total = sum(total_q, na.rm=TRUE)) %>%
    mutate(prop = total_q / total) %>%
    mutate(importer = case_when(
      prop < prop_flow_cutoff ~ "Other",
      TRUE ~ importer
    ))
  
  # country names of producers, exporters, importers
  prop_cutoff_partners <- unique(
    c(producers$producer, 
      exporters$exporter, 
      importers$importer)
  )
  
  # Creating dataframe from sankey----------------------------------------------
  
  sankey_df <- links %>%
    # Filtering data based on prop flow cutoff
    filter(producer %in% prop_cutoff_partners,
           exporter %in% prop_cutoff_partners,
           importer %in% prop_cutoff_partners)
  
  # Get country names if regions are not requested
  if (is.na(regions)) {
    sankey_df <- sankey_df %>%
      left_join(
        owid_regions %>%
          select(code, producer_name = country_name),
        by = c("producer" = "code")
      ) %>%
      left_join(
        owid_regions %>%
          select(code, exporter_name = country_name),
        by = c("exporter" = "code")
      ) %>%
      left_join(
        owid_regions %>%
          select(code, importer_name = country_name),
        by = c("importer" = "code")
      ) %>%
      # Cleaning and re-summarizing
      mutate(
        producer_name = case_when(
          is.na(producer_name) ~ "Other",
          TRUE ~ producer_name
        ),
        exporter_name = case_when(
          is.na(exporter_name) ~ "Other",
          TRUE ~ exporter_name
        ),
        importer_name = case_when(
          is.na(importer_name) ~ "Other",
          TRUE ~ importer_name
        ),
      ) %>%
      group_by(producer_name, exporter_name, importer_name) %>%
      summarize(total_q = sum(total_q, na.rm = TRUE)) %>%
      ungroup() %>%
      rename(producer = producer_name, exporter = exporter_name, importer = importer_name)
  }
  
  sankey_df <- sankey_df %>%
    # Tranforming into ggsankey format (x, node, next_x, next_node)
    make_long(producer, exporter, importer, value = total_q)
  
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
