#' Convert country ISO3c codes to region name column
#' 
#' `add_region()` takes an ISO 3166-1 alpha-3 codes column and creates a new column with corresponding region names using OWID table.
#' 
#' @param data dataframe. Must contain chr column of ISO3c codes
#' @param col character. Column name within data that contains ISO3c codes.
#' @param region.col.name character. User specified name of new region name column. Default will take input col and add "_name" for the new column name.
#' @examples
#' 
#' # Convert importer column codes to new column of region names
#' glimpse(mini_artis %>% 
#'           add_region("importer_iso3c"))
#'           
#' # Provide new column name
#' glimpse(mini_artis %>% 
#'           add_region("importer_iso3c",
#'                       region.col.name = "test_column_name"))
#' 
#' @import dplyr
#' @export
add_region <- function(data, col, region.col.name = NA){
  if(is.na(region.col.name) == TRUE){
    region.col.name <- paste(col, "_region", sep = "")
  }else{
    region.col.name <- region.col.name
  }
  
  data <- data %>%
    rename(source_var_col = {{ col }}) %>%
    left_join(
      owid_regions %>%
        select(code, region),
      by = c("source_var_col" = "code")) %>%
    mutate(region = case_when(
      source_var_col == "unknown" ~ "Unknown",
      TRUE ~ region
    ))
  
  colnames(data)[colnames(data) == "source_var_col"] <- col
  colnames(data)[colnames(data) == "region"] <- region.col.name
  
  return(data)
}
