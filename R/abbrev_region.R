#' @import tidyverse
#' @export

abbrev_region <- function(df_with_region) {
  # Take dataframe with importer_region and exporter_region columns and abbreviate the region names
  
  df_with_region<- df_with_region %>%
    # Shorten names for plotting
    mutate(importer_region = case_when(importer_region == "East Asia & Pacific" ~ "EAsia/Pac",
                                       importer_region == "Europe & Central Asia" ~ "Euro/CAsia",
                                       importer_region == "Latin America & Caribbean" ~ "LAmer/Car",
                                       importer_region == "Middle East & North Africa" ~ "MidE/NAfr",
                                       importer_region == "North America" ~ "NAmer",
                                       importer_region == "South Asia" ~ "SAsia",
                                       importer_region == "Sub-Saharan Africa" ~ "SSAfr",
                                       TRUE ~ importer_region)) %>%
    mutate(exporter_region = case_when(exporter_region == "East Asia & Pacific" ~ "EAsia/Pac",
                                       exporter_region == "Europe & Central Asia" ~ "Euro/CAsia",
                                       exporter_region == "Latin America & Caribbean" ~ "LAmer/Car",
                                       exporter_region == "Middle East & North Africa" ~ "MidE/NAfr",
                                       exporter_region == "North America" ~ "NAmer",
                                       exporter_region == "South Asia" ~ "SAsia",
                                       exporter_region == "Sub-Saharan Africa" ~ "SSAfr",
                                       TRUE ~ exporter_region))
  return(df_with_region)
} 