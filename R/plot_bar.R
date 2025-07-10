#' ARTIS Bar Chart
#' 
#' Function that creates a horizontal bar chart for ARTIS or consumption data.
#' 
#' @param data dataframe. ARTIS or consumption data.
#' @param bar_group character. Column name in data to group data into bars in plot (e.g. "sciname").
#' @param value character. Column name in data for the values the bar length.
#' @param fill_type character. Column name to use to stack the bars (e.g. "method" or "habitat"). 
#' @param top_n numeric. Number of top ranked groups to plot. 
#' @param y.lab character. Text for the y-axis label (e.g. "My title"). 
#' @param x.lab character. Text for the x-axis label. 
#' @param fill.lab character. Text for the fill legend label. 
#' @param plot.title character. Text for the plot title. 
#' @param facet_variable character. Column name to facet by.
#' @param facet_values number of facets or vector of facet categories to include. Must be specifid if a facet_variable is specified. 
#' @examples 
#'# Use `mini_artis` dataframe included in package
#'
#'# Basic plot with importer columns
#'plot_bar(mini_artis, 
#'         bar_group = "importer_iso3c")
#' 
#'# Importer bars filled by "habitat" grouping 
#'plot_bar(mini_artis, 
#'         bar_group = "importer_iso3c", 
#'         fill_type = "habitat")
#'
#'# Importer bars faceted by "habitat"
#'plot_bar(mini_artis, 
#'         bar_group = "importer_iso3c", 
#'         facet_variable  = "habitat", 
#'         facet_values = 3)
#' 
#' @import tidyverse
#' @import countrycode
#' @import viridis
#' @import tidytext
#' @export

plot_bar <- function(data, 
                     bar_group, 
                     value = "live_weight_t",
                     fill_type = NA, 
                     top_n = 10, 
                     y.lab = "", 
                     x.lab = NA, 
                     fill.lab = "",
                     plot.title = "", 
                     facet_variable = NA, 
                     facet_values = NA){
  
  if (is.na(bar_group)) {
    warning("please select a valid bar group to plot.")
    return(NULL)
  } else if (!is.na(facet_variable)) {
    
    if (bar_group == facet_variable) {
      warning("bar group and facet variable cannot be the same.")
      return(NULL)
    }
  }
  
  # data should be an ARTIS data frame
  # of the total trade are lumped together as "Other"
  
  # Setting up parameters based on user input-----------------------------------
  # Select value column to plot
  quantity <- value
  
  # If no x-axis label is provided try to provide a default option
  if (is.na(x.lab)) {
    if(quantity == "live_weight_t"){
      x.lab <- "Quantity (t live weight)"
    } else if (quantity == "product_weight_t") {
      x.lab <- "Quantity (t product weight)"
    } else {
      x.lab <- ""
    }
  }
  
  # If no fill label is provided, try to 
  if (!is.na(fill_type)) {
    if (fill_type == "dom_source") {
      fill.lab <- "Export Source"
    } else if (fill_type == "method") {
      fill.lab <- "Production Method"
    } 
  }

  # Rename indicated bar group based on bar_group argument
  data <- data %>%
    rename("bar_group" = .data[[bar_group]])
  
  
  # Factors for bar ordering----------------------------------------------------
  if("dom_source" %in% colnames(data)){
    data$dom_source <- factor(data$dom_source,
                              levels = c("domestic", "foreign", "error"))
  }

  if("method" %in% colnames(data)){
    data$method <- factor(data$method,
                          levels = c("aquaculture", "capture", "unknown"))
  }
  
  if("habitat" %in% colnames(data)){
    data$habitat <- factor(data$habitat,
                          levels = c("marine", "inland", "unknown"))
  }
  
  # Summarizing data by bar group, fill type and facetting group
  grouping_cols <- c("bar_group")
  
  if (!is.na(fill_type)) {
    grouping_cols <- c(grouping_cols, fill_type)
  }
  
  if (!is.na(facet_variable)) {
    grouping_cols <- c(grouping_cols, facet_variable)
  }
  
  data <- data %>%
    group_by(across(grouping_cols)) %>%
    summarise(quantity = sum(.data[[quantity]], na.rm=TRUE)) %>%
    ungroup()
  
  if (!is.na(fill_type) & is.na(facet_variable)) {
    colnames(data) <- c("bar_group", "fill_type", "quantity")
  } else if (!is.na(fill_type) & !is.na(facet_variable)) {
    colnames(data) <- c("bar_group", "fill_type", "facet_variable", "quantity")
  } else if (is.na(fill_type) & !is.na(facet_variable)) {
    colnames(data) <- c("bar_group", "facet_var", "quantity")
  }
  
  # limiting facets by values provided
  if (!is.na(facet_variable)) {
    if (typeof(facet_values) == "character") {
      data <- data %>%
        filter(facet_var %in% facet_values) %>%
        group_by(bar_group, facet_var) %>%
        mutate(total = sum(quantity, na.rm = TRUE)) %>%
        group_by(facet_var) %>%
        slice_max(order_by = total, n = top_n) %>%
        ungroup() %>%
        mutate(bar_group = reorder_within(bar_group, total, facet_var))
      
    } else if (typeof(facet_values) == "double") {
      data <- data %>%
        group_by(facet_var) %>%
        mutate(total = sum(quantity, na.rm = TRUE)) %>%
        ungroup()
      
      top_n_value <- data %>%
        select(total) %>%
        distinct() %>%
        arrange(desc(total)) %>%
        slice_max(order_by = total, n = facet_values) %>%
        pull(total) %>%
        min()
      
      data <- data %>%
        filter(total >= top_n_value) %>%
        group_by(facet_var) %>%
        slice_max(order_by = quantity, n = top_n) %>%
        ungroup() %>%
        mutate(bar_group = reorder_within(bar_group, quantity, facet_var))
      
      }
    } else if (is.na(facet_variable)) {
      # Summarizing for top n variables
      data <- data %>%
        group_by(bar_group) %>%
        mutate(total = sum(quantity, na.rm = TRUE)) %>%
        ungroup()
      
      # Top N bars in the bar chart
      top_n_value <- data %>%
        select(bar_group, total) %>%
        distinct() %>%
        slice_max(order_by = total, n = top_n) %>%
        pull(total) %>%
        min()
      
      data <- data %>%
        filter(total >= top_n_value) %>%
        mutate(bar_group = reorder(bar_group, total))
    } else {
      warning("entered invalid facet values")
      return(NULL)
    }
  
  
  # Bar Chart Creation----------------------------------------------------------
  
  p <- ggplot(data, aes(x = quantity, y = bar_group))
  
  if (!is.na(fill_type)) {
    p <- p +
      geom_bar(stat = "identity", aes(fill = fill_type)) +
      scale_fill_manual(values = artis_palette(length(unique(data$fill_type))))
  } else {
    p <- p +
      geom_bar(stat = "identity", fill = "#114F59")
  }
  
  if (!is.na(facet_variable)) {
    p <- p +
      facet_wrap(as.formula(paste(".~", "facet_var")), scales = "free") +
      tidytext::scale_y_reordered()
  }
  
  # add title, axis labels, and fill legend
  if (plot.title != "") {
    p <- p + labs(
      title = plot.title,
      y     = y.lab,
      x     = x.lab,
      fill  = fill.lab
    )
  } else {
    p <- p + labs(
      y    = y.lab,
      x    = x.lab,
      fill = fill.lab
    )
  }

  p <- p + theme_bw()

  return(p)
  }
