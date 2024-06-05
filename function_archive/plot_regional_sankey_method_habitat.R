
#' @import tidyverse
#' @import ggsankey
#' @import ggpubr
#' @export

plot_regional_sankey_method_habitat <- function(df, start_year, end_year) {
  
  df <- df %>%
    filter(year >= start_year & year <= end_year) %>%
    filter(exporter_region != "Other" & importer_region != "Other")
  
  # Dataframe for CAPTURE MARINE sankey diagram--------------------------------
  capture_marine <- df %>%
    filter(method == "capture" & habitat == "marine") %>%
    group_by(exporter_region, importer_region) %>%
    summarize(live_weight_t = sum(live_weight_t, na.rm = TRUE)) %>%
    ungroup() %>%
    make_long(exporter_region, importer_region, value = live_weight_t)
  
  capture_marine$node <- factor(capture_marine$node,
                                levels = c("Europe", "Oceania", "Asia",
                                           "South America", "North America",
                                           "Africa"))
  
  # Dataframe for CAPTURE INLAND sankey diagram---------------------------------
  capture_inland <- df %>%
    filter(method == "capture" & habitat == "inland") %>%
    group_by(exporter_region, importer_region) %>%
    summarize(live_weight_t = sum(live_weight_t, na.rm = TRUE)) %>%
    ungroup() %>%
    make_long(exporter_region, importer_region, value = live_weight_t)
  
  capture_inland$node <- factor(capture_inland$node,
                                levels = c("Europe", "Oceania", "Asia",
                                           "South America", "North America",
                                           "Africa"))
  
  # Dataframe for AQUACULTURE MARINE sankey diagram-----------------------------
  aqua_marine <- df %>%
    filter(method == "aquaculture" & habitat == "marine") %>%
    group_by(exporter_region, importer_region) %>%
    summarize(live_weight_t = sum(live_weight_t, na.rm = TRUE)) %>%
    ungroup() %>%
    make_long(exporter_region, importer_region, value = live_weight_t)
  
  aqua_marine$node <- factor(aqua_marine$node,
                                levels = c("Europe", "Oceania", "Asia",
                                           "South America", "North America",
                                           "Africa"))
  
  # Dataframe for AQUACULTURE INLAND sankey diagram-----------------------------
  aqua_inland <- df %>%
    filter(method == "aquaculture" & habitat == "inland") %>%
    group_by(exporter_region, importer_region) %>%
    summarize(live_weight_t = sum(live_weight_t, na.rm = TRUE)) %>%
    ungroup() %>%
    make_long(exporter_region, importer_region, value = live_weight_t)
  
  aqua_inland$node <- factor(aqua_inland$node,
                                levels = c("Europe", "Oceania", "Asia",
                                           "South America", "North America",
                                           "Africa"))
  
  # CAPTURE MARINE sankey-------------------------------------------------------
  capture_marine_p <- capture_marine %>%
    ggplot(aes(x = x, 
               next_x = next_x, 
               node = node, 
               next_node = next_node,
               fill = factor(node),
               value = value,
               label = node)) +
    geom_sankey(flow.alpha = 0.6) +
    scale_fill_manual(values = region6_palette) +
    geom_sankey_label(size = 3, color = "white", fill = "gray40") +
    theme_sankey(base_size = 18) +
    labs(title = "Marine", x = NULL, y = "CAPTURE") +
    theme(
      legend.position = "none",
      axis.text.x = element_blank(),
      plot.title = element_text(hjust = 0.5)
      )
  
  # CAPTURE INLAND sankey-------------------------------------------------------
  capture_inland_p <- capture_inland %>%
    ggplot(aes(x = x, 
               next_x = next_x, 
               node = node, 
               next_node = next_node,
               fill = factor(node),
               value = value,
               label = node)) +
    geom_sankey(flow.alpha = 0.6) +
    scale_fill_manual(values = region6_palette) +
    geom_sankey_label(size = 3, color = "white", fill = "gray40") +
    theme_sankey(base_size = 18) +
    labs(title = "Inland", x = NULL) +
    theme(
      legend.position = "none",
      axis.text.x = element_blank(),
      #plot.margin=unit(c(0,0,0,0), "mm"),
      plot.title = element_text(hjust = 0.5)
    )
  
  # AQUACULTURE MARINE sankey---------------------------------------------------
  aqua_marine_p <- aqua_marine %>%
    ggplot(aes(x = x, 
               next_x = next_x, 
               node = node, 
               next_node = next_node,
               fill = factor(node),
               value = value,
               label = node)) +
    geom_sankey(flow.alpha = 0.6) +
    scale_fill_manual(values = region6_palette) +
    geom_sankey_label(size = 3, color = "white", fill = "gray40") +
    theme_sankey(base_size = 18) +
    labs(x = NULL, y = "AQUACULTURE") +
    theme(
      legend.position = "none",
      axis.text.x = element_blank()#,
      #plot.margin=unit(c(0,0,0,0), "mm")
    )
  
  # AQUACULTURE INLAND sankey---------------------------------------------------
  aqua_inland_p <- aqua_inland %>%
    ggplot(aes(x = x, 
               next_x = next_x, 
               node = node, 
               next_node = next_node,
               fill = factor(node),
               value = value,
               label = node)) +
    geom_sankey(flow.alpha = 0.6) +
    scale_fill_manual(values = region6_palette) +
    geom_sankey_label(size = 3, color = "white", fill = "gray40") +
    theme_sankey(base_size = 18) +
    labs(x = NULL) +
    theme(
      legend.position = "none",
      axis.text.x = element_blank()#,
      #plot.margin=unit(c(0,0,0,0), "mm")
    )
  
  # Sankey of all method habitat combinations-----------------------------------
  ggarrange(capture_marine_p, capture_inland_p,
            aqua_marine_p, aqua_inland_p,
            ncol = 2, nrow = 2)#,
            # padding = 0,
            # legend = "none")
}
