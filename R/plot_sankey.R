#' Creates a Sankey plot of ARTIS data
#' 
#' This is a function that creates a sankey plot showcasing seafood supply chains from producer to importer in the ARTIS dataset.
#' 
#' @param data an ARTIS dataframe.
#' @param cols ADD DESCRIPTION
#' @param prop_flow_cutoff default prop_flow_cutoff = 0.05 means trade volumes that comprise less than 5\% of the total trade are lumped together as "Other".
#' @param weight trade quantity type to visualize ("live" for live weight or "product" for product weight), default "live."
#' @return None
#' @import tidyverse
#' @import countrycode
#' @import ggsankey
#' @export
plot_sankey <- function(data, cols = c("source_country_iso3c", "exporter_iso3c", "importer_iso3c"), 
                        prop_flow_cutoff = 0.05, 
                        export_source = NA, 
                        weight = "live_weight_t", show.other = FALSE, 
                        plot.title = "") {
  
  # Warn user if NAs are present in the selected columns
  if (sum(is.na(data[cols])) > 0) {
    warning("WARNING: The selected columns include NA values")
  }
  
  # Setting up parameters based on user input-----------------------------------
  
  # Assign weight column to quantity
  quantity <- weight

  # Summarizing data based on quantity variable selected
  links <- data %>%
    group_by_at(vars(cols)) %>%
    summarize(total_q = sum(.data[[quantity]], na.rm = TRUE))

  # Renaming for consistency
  colnames(links) <- c(paste("col_", 1:length(cols), sep = ""), "total_q")
  cols <- colnames(links)[1:length(cols)]

  # Identify list of nodes by column by proportional flow cutoff
  node_names <- c()
  
  for(i in 1:length(cols)){
    node_i <- links %>%
      rename(col_i = paste("col_", i, sep = "")) %>%
      group_by(col_i) %>%
      summarize(total_q = sum(total_q, na.rm=TRUE)) %>%
      ungroup() %>%
      mutate(total = sum(total_q, na.rm=TRUE)) %>%
      mutate(prop = total_q / total) %>%
      mutate(name_new = case_when(
        prop < prop_flow_cutoff ~ "Other",
        TRUE ~ col_i
      ))
    node_names_i <- node_i$name_new
    
    node_names <- c(node_names, node_names_i)
  }
  
  node_names <- unique(node_names)
  
  # Creating dataframe from sankey----------------------------------------------
  
  sankey_df <- links %>%
    # Filtering data based on prop flow cutoff
    filter_at(vars(cols), all_vars(. %in% node_names)) %>%
    ungroup()
  
  sankey_df <- sankey_df %>%
    # Transforming into ggsankey format (x, node, next_x, next_node)
    make_long({{ cols }}, value = total_q)
  
  num_nodes <- length(unique(c(sankey_df$node,sankey_df$next_node)))
  
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
    scale_fill_manual(values = artis_palette(num_nodes)) + 
    labs(x = NULL, title = plot.title) +
    theme(
      legend.position = "none",
      axis.text.x = element_blank()
    )
}
