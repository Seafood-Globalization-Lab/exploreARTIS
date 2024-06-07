#' Convert country ISO3c codes to name column
#' 
#' `add_country_name()` takes an ISO 3166-1 alpha-3 codes column and creates a new column with full country names using OWID table.
#' 
#' @param data dataframe. Must contain column of ISO3c codes
#' @param col character. Column name within data that cont
#' @param country.col.name character. User specified name of new country name column. Default will take input col and add "_name" for the new column name.
#' @examples
#' 
#' # Convert importer column codes to new column of full names
#' glimpse(mini_artis %>% 
#'           add_country_name("importer_iso3c"))
#' 
#' # Provide new column name
#' 
#' glimpse(mini_artis %>% 
#'           add_country_name("importer_iso3c",
#'                             country.col.name = "test_column_name"))
#' 
#' @import dplyr
#' @export

add_country_name <- function(data, 
                             col, 
                             country.col.name = NA){
  if(is.na(country.col.name) == TRUE){
    country.col.name <- paste(col, "_name", sep = "")
  }else{
    country.col.name <- country.col.name
  }
  
  data <- data %>%
    rename(source_var_col = {{ col }}) %>%
    left_join(
      owid_regions %>%
        select(code, country_name),
      by = c("source_var_col" = "code")) %>%
    mutate(country_name = case_when(
      source_var_col == "unknown" ~ "Unknown",
      TRUE ~ country_name
    ))
  
  colnames(data)[colnames(data) == "source_var_col"] <- col
  colnames(data)[colnames(data) == "country_name"] <- country.col.name
  
  return(data)
  
}