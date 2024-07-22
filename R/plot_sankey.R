#' Sankey plot of ARTIS data
#'
#' A function to create a Sankey plot showcasing elements of seafood supply chains in ARTIS or consumption datasets.
#' 
#' @param data dataframe. An ARTIS trade or consumption dataframe.
#' @param cols vector. Column names to generate the sections of the Sankey plot, in the order they should appear (left to right).
#' @param prop_flow_cutoff integer. A percent in which trade volumes that comprise less than x\% of the total trade are renamed as "Other". Default prop_flow_cutoff = 0.05 means trade volumes less than 5\% are labeled as "Other".
#' @param value character. Trade quantity column name to visualize. Default is "live_weight_t".
#' @param show.other logical. Controls whether or not nodes within a column falling below the prop_flow_cutoff threshold should be displayed in a group ("Other"). Default value is TRUE, filtering for threshold occurs regardless if "Other" is displayed.
#' @param plot.title character. User-specified title for the plot. Default is a blank title.
#' @return A Sankey plot.
#' @examples
#' # Use `mini_artis` dataframe included in package
#'
#' # Basic default Sankey plot
#' plot_sankey(mini_artis)
#'
#' # Define alternative columns to use in Sankey plot
#' plot_sankey(mini_artis, 
#'             cols = c("method", "habitat"))
#'
#' # Retain all flows - no filtering value by proportion cutoff 
#' plot_sankey(mini_artis,
#'             prop_flow_cutoff = 0)
#'             
#' @import ggplot2
#' @import dplyr
#' @import tidyr
#' @import ggsankey
#' @export

plot_sankey <- function(data, 
                        cols = c("source_country_iso3c", 
                                 "exporter_iso3c", 
                                 "importer_iso3c"), 
                        prop_flow_cutoff = 0.05, 
                        value = "live_weight_t", 
                        show.other = TRUE, 
                        plot.title = "") {
  
  # Warn user if NAs are present in the selected columns
  if (sum(is.na(data[cols])) > 0) {
    warning("WARNING: The selected columns include NA values")
  }
  
  # Setting up parameters based on user input-----------------------------------
  
  # Assign weight column to quantity
  quantity <- value

  # Summarizing data based on quantity variable selected
  links <- data %>%
    group_by_at(vars(cols)) %>% # vars() deprecated
    summarize(total_q = sum(.data[[quantity]], na.rm = TRUE))

  # Rename specified columns to generic names for processing
  colnames(links) <- c(paste("col_", 1:length(cols), sep = ""), "total_q")
  cols <- colnames(links)[1:length(cols)]

  # Identify list of nodes by column by proportional flow cutoff
  node_names <- c()
  
  for(i in 1:length(cols)){
    # Identify nodes in the column falling below the threshold
    node_i <- links %>%
      rename(col_i = paste("col_", i, sep = "")) %>%
      group_by(col_i) %>%
      summarize(total_q = sum(total_q, na.rm=TRUE)) %>%
      ungroup() %>%
      mutate(total = sum(total_q, na.rm=TRUE)) %>%
      mutate(prop = total_q / total) %>%
      # label those below threshold as "Other" - otherwise keep name
      mutate(name_new = case_when(
        prop < prop_flow_cutoff ~ "Other",
        TRUE ~ col_i
      ))
    
    # Create vector of nodes
    node_names_i <- node_i$name_new
    
    # Replace node names with "Other" when it falls below the threshold 
    # in links dataframe
    links <- links %>%
      rename(col_i = paste("col_", i, sep = "")) %>%
      mutate(col_i = case_when(
        col_i %in% node_names_i ~ col_i,
        TRUE ~ "Other"
      ))
    # rename working column with generic name perscribed above 
    colnames(links)[colnames(links) == "col_i"] <- paste("col_", i, sep = "")
    
    # vector of all node names
    node_names <- unique(c(node_names, node_names_i))
  }
  # vector of all node names across all columns of interest 
  node_names <- unique(node_names)
  
  # Creating dataframe from sankey----------------------------------------------
  # keep "Other" observation values if specified, remove if flase
  if(show.other == FALSE){
    node_names <- node_names[node_names != "Other"]
  }else{
    node_names <- node_names
  }
  
  sankey_df <- links %>%
    # Filtering data based on prop flow cutoff - use show.other value 
    filter_at(vars(cols), all_vars(. %in% node_names)) %>%
    ungroup()
  
  sankey_df <- sankey_df %>%
    # Transforming into ggsankey format (x, node, next_x, next_node)
    ggsankey::make_long({{ cols }}, value = total_q)
  
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
      axis.text.x = element_text()
    )
}
