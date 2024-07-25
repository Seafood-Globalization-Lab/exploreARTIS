#' Creates a time series line graph/stacked area graph
#' 
#' @param data an ARTIS trade or consumption data frame.
#' @param artis_var variable for color or fill groups. 
#' @param plot.type select either "line" or "stacked" for the plot type. Default is "line."
#' @param prop_flow_cutoff default prop_flow_cutoff = 0.05 means trade volumes that comprise less than 5\% of the total trade are lumped together as "Other".
#' @param value trade quantity type to visualize ("live" for live weight or "product" for product weight), default "live."
#' @param y.lab text for the y-axis label. 
#' @param legend.title text for the color or fill legend label. 
#' @param plot.title text for the plot title. 
#' @param facet_variable variable name to facet by.
#' @param facet_n number of facets to include. Must be specifid if a facet_variable is specified. 
#' @import tidyverse
#' @import countrycode
#' @import viridis
#' @export
plot_ts <- function(data, 
                    artis_var = NA, 
                    plot.type = "line",
                    prop_flow_cutoff = 0.05,
                    value = "live_weight_t", 
                    facet_variable = NA, facet_values = NA,
                    y.lab = NA, legend.title = NA, plot.title = "") {
  
  # Specify warnings for users
  if(is.na(artis_var)) {
    warning("please select a variable to plot")
    return(NULL)
  }
  
  if (!is.na(facet_variable)) {
    if (artis_var == facet_variable) {
      warning("artis_var cannot be the same as facet_variable")
      return(NULL)
    }
  }
  
  #-----------------------------------------------------------------------------
  # Initial variable setup
  quantity <- value
  
  # If no quantity (y-axis) label is provided try to provide a default option
  if (is.na(y.lab)) {
    if(quantity == "live_weight_t"){
      y.lab <- "Quantity (t live weight)"
    } else if (quantity == "product_weight_t") {
      y.lab <- "Quantity (t product weight)"
    } else {
      y.lab <- ""
    }
  }
  
  # Labels for Legend
  # Default legend title will be the one provided
  # Otherwise will try to provide a standardized name based on ARTIS variable chosen
  # If legend title cannot be determined by ARTIS variable then it will be blank
  if (!is.na(legend.title)) {
    color.lab <- legend.title
  } else {
    if (artis_var == "method") {
      color.lab <- "Production Method"
    } else if (artis_var == "dom_source") {
      color.lab <- "Export Source"
    } else if (artis_var == "hs6") {
      color.lab <- "HS Product"
    } else if (artis_var == "exporter_iso3c") {
      color.lab <- "Exporters"
    } else if (artis_var == "importer_iso3c") {
      color.lab <- "Importers"
    } else if (artis_var == "sciname") {
      color.lab <- "Species/Species Group"
    } else {
      color.lab <- ""
    }
  }

  #-----------------------------------------------------------------------------
  # Summarize data based on variable selected
  grouping_cols <- c("year", artis_var)
  
  if (!is.na(facet_variable)) {
    grouping_cols <- c(grouping_cols, facet_variable)
  }
  
  # Getting time series of data by variable selected
  data <- data %>%
    group_by(across(grouping_cols)) %>%
    summarize(quantity = sum(.data[[quantity]], na.rm = TRUE)) %>%
    ungroup()
  
  if (!is.na(facet_variable)) {
    colnames(data) <- c("year", "variable", "facet_var", "quantity")
  } else {
    colnames(data) <- c("year", "variable", "quantity")
  }
  
  # limiting facets by values provided
  if (!is.na(facet_variable)) {
    if (typeof(facet_values) == "character") {
      data <- data %>%
        filter(facet_var %in% facet_values)
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
        filter(total >= top_n_value) 
      
    } else {
      warning("entered invalid facet values")
      return(NULL)
    }
  }
  
  # Checking against prop_flow cutoff if necessary
  prop_flow_cols <- c("year")
  if(!is.na(facet_variable)) {
    prop_flow_cols <- c(prop_flow_cols, "facet_var")
  }
  
  if (prop_flow_cutoff != 0) {
    # Calculate proportion across the whole time series
    
    # Case when there is no facet variable for the time series
    if (!("facet_var" %in% prop_flow_cols)) {
      data <- data %>%
        # Total across all years
        mutate(total = sum(quantity, na.rm = TRUE)) %>%
        # Total by variable group and find proportion across whole timeseries
        group_by(variable) %>%
        mutate(var_total = sum(quantity, na.rm = TRUE)) %>%
        ungroup() %>%
        # Calculate proportion of variable across whole timeseries
        # (ie total by importer across whole timeseries / total imports of whole timeseries)
        mutate(prop = var_total / total) %>%
        # All variable totals below a the proportional flow cutoff get renamed to "Other"
        mutate(variable = case_when(
          prop < prop_flow_cutoff ~ "Other",
          TRUE ~ variable
        ))
    } else {
      # Case when there is a variable for the time series
      # Total across all years by facet variable
      # FIXIT: include an additional function argument "facet_free" to allow each facet to have its own distinct set of top fill variables
      # (ie Top importers facetted by production habitat, where each facet has a distinct set of top importers)
      data <- data %>%
        group_by(facet_var) %>%
        # Total across all years BY facet variable
        mutate(facet_total = sum(quantity, na.rm = TRUE)) %>%
        # Total by variable group AND facet variable and find proportion across each group
        group_by(variable, facet_var) %>%
        mutate(var_total = sum(quantity, na.rm = TRUE)) %>%
        ungroup() %>%
        # Calculate proportion of variable across facet groups
        # (ie total by importer by each facet group / total imports of by each facet group)
        mutate(prop = var_total / facet_total) %>%
        # All variable totals below a the proportional flow cutoff get renamed to "Other"
        mutate(variable = case_when(
          prop < prop_flow_cutoff ~ "Other",
          TRUE ~ variable
        ))
    }
    
    prop_flow_cols <- unique(c(prop_flow_cols, "variable"))
    
    data <- data %>%
      # Re-summarize based on some categories turned to "Other"
      group_by(across(prop_flow_cols)) %>%
      summarize(quantity = sum(quantity, na.rm = TRUE)) %>%
      ungroup()
  }
  
  # Filling in missing values for any years with zeros
  df_variables <- c()
  if (!is.na(facet_variable)) {
    df_variables <- c("year", "variable", "facet_var")
    year_grid <- expand_grid(year = unique(data$year), 
                             variable = unique(data$variable),
                             facet_var = unique(data$facet_var))
  } else {
    df_variables <- c("year", "variable")
    year_grid <- expand_grid(year = unique(data$year), 
                             variable = unique(data$variable))
  }
  
  data <- data %>%
    full_join(year_grid,
              by = df_variables) %>%
    mutate(quantity = if_else(is.na(quantity), true = 0, false = quantity))
  
  if (artis_var == "sciname") {
    # Format scinames for presentation
    data <- data %>%
      mutate(variable = str_to_sentence(variable))
  }
  
  # Reorder (descending) based on quantity
  if (!is.na(facet_variable)) {
    data <- data %>%
      group_by(year, facet_var) %>%
      mutate(variable = fct_reorder(variable, quantity)) %>%
      ungroup()
  } else {
    data <- data %>%
      group_by(year) %>%
      mutate(variable = fct_reorder(variable, quantity)) %>%
      ungroup()
  }
  
  
  # Reorder so that "Other" always last
  if ("Other" %in% unique(data$variable)) {
    data <- data %>%
      mutate(variable = forcats::fct_relevel(variable, "Other", after = Inf))
  }
  
  #-----------------------------------------------------------------------------
  # Visualizing timeseries as line graphs
  
  if (plot.type == "line") {
    
    p <- data %>%
      ggplot(aes(x = year, y = quantity, color = variable)) +
      geom_line(size = 1.1) +
      scale_color_manual(values = artis_palette(length(unique(data$variable)))) +
      labs(y = y.lab, x = "Year", title = plot.title, color = color.lab) +
      theme_bw()
  } else {
    p <- data %>%
      ggplot() +
      geom_area(aes(x = year, y = quantity, fill = variable)) +
      scale_fill_manual(values = artis_palette(length(unique(data$variable)))) +
      labs(y = y.lab, x = "Year", title = plot.title, fill = color.lab) +
      theme_bw() 
  }
  
  if (!is.na(facet_variable)) {
    p <- p + 
      facet_wrap(as.formula(paste(".~", "facet_var")))
  }
  
  return(p)
}
