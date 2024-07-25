#' Creates a chloropleth map with trade flows of ARTIS data
#' 
#' This is a function that creates a chloropleth map (optional) with trade flow arrows (optional)  based on the ARTIS dataset.
#' 
#' @param data an ARTIS dataframe.
#' @param value the numeric variable for the fill/color. Default is live_weight_t.  
#' @param country_fill optional variable by which to color countries - either "import" to color by total imports or export to color by total export.
#' @param flow_arrows logical variable - set to TRUE to include top trade flow arrows. 
#' @param n_flows number of top trade flow arrows to include if flow_arrows = TRUE - defaults to 10. 
#' @return None
#' @import tidyverse
#' @import viridis
#' @import rnaturalearth
#' @import countrycode
#' @importFrom ggthemes theme_map
#' @export

plot_map <- function(data,
                     value = "live_weight_t",
                     country_fill = NA, flow_arrows = FALSE, n_flows = 10,
                     arrow_label = NA, fill_label = NA, caption_label = NA,
                     regions = FALSE){
  
  # Load world map data
  world <- ne_countries(scale = "medium", returnclass = "sf") %>%
    filter(iso_a3 != "ATA")
  
  if (is.na(country_fill)) {
    trade_map <- ggplot(world) +
      geom_sf(size = 0.1, color = "white", fill = "#97acb7", alpha = 1)
  } else {
    trade_map <- ggplot(world) +
      geom_sf(size = 0.1, color = "white", fill = "grey", alpha = 0.5)
  }
  
  
  # If country_fill is provided, create import or export value to fill by
  if(is.na(country_fill) == FALSE){
    country_fill_col <- country_fill
    
    # Summarize total export or import data
    chloropleth_df <- data %>%
      group_by(.data[[country_fill_col]]) %>% 
      summarize(quantity = sum(.data[[value]], na.rm=TRUE)/1000000) %>%
      rename("iso_a3" = paste(country_fill_col))
    
    # Summarizing by region if requested
    if (regions) {
      chloropleth_df <- chloropleth_df %>%
        left_join(
          owid_regions %>%
            select(code, region),
          by = c("iso_a3" = "code")
        ) %>%
        filter(!is.na(region)) %>%
        group_by(region) %>%
        mutate(quantity = sum(quantity, na.rm = TRUE)) %>%
        ungroup() %>%
        left_join(world, by = "iso_a3")
      
    } else {
      chloropleth_df <- chloropleth_df %>%
        left_join(world, by = "iso_a3")
    }
    
    trade_map <- trade_map +
      geom_sf(data = chloropleth_df, 
              aes(fill = quantity, geometry = geometry), color = "white", size = 0.1) +
      scale_fill_gradient(low = "#86ADA7", high = "#0F2D59") +
      labs(fill = fill_label)
    
  }else{
    trade_map <- trade_map
  }
  
  if(flow_arrows == TRUE){
    
    flows_df <- data %>%
      group_by(importer_iso3c, exporter_iso3c) %>% 
      summarize(quantity = sum(.data[[value]], na.rm=TRUE)/1000000) %>%
      ungroup()
    
    # Summarizing centroids by region if requested
    if (regions) {
      
      flows_df <- flows_df %>%
        left_join(
          owid_regions %>%
            select(code, exporter_region = region),
          by = c("exporter_iso3c" = "code")
        ) %>%
        left_join(
          owid_regions %>%
            select(code, importer_region = region),
          by = c("importer_iso3c" = "code")
        ) %>%
        # Remove NEI since NEI does not get classified into a region
        filter(!is.na(exporter_region) & !is.na(importer_region)) %>%
        # Summarize by region
        group_by(exporter_region, importer_region) %>%
        summarize(quantity = sum(quantity, na.rm = TRUE)) %>%
        ungroup() %>%
        # Remove regional self loops for arrows
        filter(exporter_region != importer_region) %>%
        # Get regional centroids
        left_join(
          owid_centroids,
          by = c("exporter_region" = "region")
        ) %>%
        left_join(
          owid_centroids,
          by = c("importer_region" = "region")
        ) %>%
        slice_max(n = n_flows, order_by = quantity)
      
    } else {
      
      flows_df <- flows_df %>%
        slice_max(n = n_flows, order_by = quantity) %>%
        # Join with lat/long data for centroids
        left_join(country_centroids, by = c("exporter_iso3c" = "iso3")) %>%
        left_join(country_centroids, by = c("importer_iso3c" = "iso3"))
      
      flows_df <- flows_df %>%
        mutate(
          centroid.lat.x = case_when(
            exporter_iso3c %in% c("USA", "CHN", "RUS") ~ 1.05 * centroid.lat.x,
            TRUE ~ centroid.lat.x),
          centroid.lat.y = case_when(
            importer_iso3c %in% c("USA", "CHN", "RUS") ~ 0.95 * centroid.lat.y,
            TRUE ~ centroid.lat.y
          ))
    }
    
    trade_map <- trade_map +
      geom_curve(data = flows_df, 
                 aes(x = centroid.lon.x, y = centroid.lat.x, 
                     xend = centroid.lon.y, yend = centroid.lat.y,
                     color = quantity),
                 size = 1,
                 alpha = 0.75,
                 curvature = -0.35, arrow = arrow(length = unit(3, "mm"), angle = 20)) +
      scale_colour_gradient(low = "#F7AF75", high = "#E24027") +
      labs(color = arrow_label)
  }
  
  trade_map <- trade_map + 
    theme_map() + 
    theme(legend.position = "bottom", legend.box="vertical", plot.background = element_rect(fill = "white"),
          legend.margin=margin(), legend.text = element_text(size=10),
          legend.title = element_text(size=11)) +
    guides(size = "none", fill = guide_colorbar(barwidth = 10, barheight = 0.75), 
           color = guide_colorbar(barwidth = 10, barheight = 0.75))
  
  if (!is.na(caption_label)) {
    trade_map <- trade_map +
      labs(caption = caption_label) +
      theme(plot.caption = element_text(face = "italic"))
  }
    
  return(trade_map)
}

