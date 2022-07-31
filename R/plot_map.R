#' Creates a chloropleth map with trade flows of ARTIS data
#' 
#' This is a function that creates a chloropleth map (optional) with trade flow arrows (optional)  based on the ARTIS dataset.
#' 
#' @param data an ARTIS dataframe.
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
#' @param country_fill optional variable by which to color countries - either "import" to color by total imports or export to color by total export.
#' @param flow_arrows logical variable - set to TRUE to include top trade flow arrows. 
#' @param n_flows number of top trade flow arrows to include if flow_arrows = TRUE - defaults to 10. 
#' @return None
#' @import tidyverse
#' @import viridis
#' @import sf
#' @import rnaturalearth
#' @import CoordinateCleaner
#' @import cowplot
#' @export

plot_map <- function(data,
                     species = NA, years = NA,
                     producers = NA, exporters = NA, importers = NA,
                     hs_codes = NA, prod_method = NA, prod_environment = NA,
                     export_source = NA, weight = "live",
                     country_fill = NA, flow_arrows = FALSE, n_flows = 10){
  # Select live or product weight
  if(weight == "live"){
    quantity <- "live_weight_t"
    quantity.lab <- "(million t live weight)"
  }else{
    quantity <- "product_weight_t"
    quantity.lab <- "(million t product weight)"
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
  
  # Load world map data
  world <- ne_countries(scale = "medium", returnclass = "sf") %>%
    filter(iso_a3 != "ATA")
  
  # Change projection
  PROJ <- "+proj=eck4 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs" 
  #world <- st_transform(world, PROJ)
  
  trade_map <- ggplot(world) +
    geom_sf(size = 0.1) 
    # FIX IT: Projection is not being applied for some reason
    #coord_sf(crs = PROJ) 
  
  # If country_fill is provided, create import or export value to fill by
  if(is.na(country_fill) == FALSE){
    # Set column for fill color
    if(country_fill == "import"){
      country_fill_col <- "importer_iso3c"
    }else{
      country_fill_col <- "exporter_iso3c"
    }
    # Summarize total export or import data
    chloropleth_df <- data %>%
      group_by(.data[[country_fill_col]]) %>% 
      summarize(quantity = sum(.data[[quantity]], na.rm=TRUE)/1000000) %>%
      rename("iso_a3" = paste(country_fill_col)) %>%
      right_join(world, by = "iso_a3")
    
    trade_map <- trade_map +
      geom_sf(data = chloropleth_df, 
              aes(fill = quantity, geometry = geometry), size = 0.1) +
      scale_fill_gradient(low = "#86ADA7", high = "#0F2D59") +
      labs(fill = paste("Total ", country_fill, " \n", quantity.lab, sep = ""))
    
  }else{
    trade_map <- trade_map
  }
  
  if(flow_arrows == TRUE){
    # Create centroids data frame
    country_centroids <- countryref %>%
      filter(str_count(adm1_code)==3) %>%
      group_by(iso3) %>%
      summarise(centroid.lon = mean(centroid.lon),
                centroid.lat = mean(centroid.lat))
    
    flows_df <- data %>%
      group_by(importer_iso3c, exporter_iso3c) %>% 
      summarize(quantity = sum(.data[[quantity]], na.rm=TRUE)/1000000) %>%
      ungroup() %>%
      slice_max(n = n_flows, order_by = quantity) %>%
      # Join with lat/long data for centroids
      left_join(country_centroids, by = c("exporter_iso3c" = "iso3")) %>%
      left_join(country_centroids, by = c("importer_iso3c" = "iso3")) 
      
    
    trade_map <- trade_map +
      geom_curve(data = flows_df, 
                 aes(x = centroid.lon.x, y = centroid.lat.x, 
                     xend = centroid.lon.y, yend = centroid.lat.y,
                     size = quantity/max(quantity),
                     color = quantity),
                 alpha = 0.75, 
                 curvature = -0.2, arrow = arrow(length = unit(0.05, "npc"), type = "closed")) +
      scale_colour_gradient(low = "#F7AF75", high = "#E24027") +
      scale_size_continuous(range = c(1, 3)) +
      labs(color = paste("Export\n", quantity.lab, sep = ""))
  }
  
  trade_map <- trade_map + 
    theme_map() + 
    theme(legend.position = "bottom", legend.box="vertical", 
          legend.margin=margin(), legend.text = element_text(size=10),
          legend.title = element_text(size=11)) +
    guides(size = "none", fill = guide_colorbar(barwidth = 10, barheight = 0.75), 
           color = guide_colorbar(barwidth = 10, barheight = 0.75))
    
  return(trade_map)
}
