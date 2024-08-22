#' Convert country ISO3c codes to name column
#' 
#' `add_country_name()` takes an ISO 3166-1 alpha-3 codes column and creates a new column with full country names using OWID table. "NEI" (i.e. nowhere else included) column values are replaced with "Nowhere Else Included" and "unknown" values are replaced with "Unknown". Will return an error if "country_name" column already exists in the dataframe. 
#' 
#' @param data dataframe. Must contain chr column of ISO3c codes
#' @param col character. Column name within data that contains ISO3c codes.
#' @param country.col.name character. User specified name of new country name column. Default will take input col and add "_name" for the new column name.
#' @examples
#' 
#' # Use `mini_artis` dataframe included in package, remove "country_name" column
#' library(dplyr)
#' test_data <- mini_artis %>% select(importer_iso3c, year, region, live_weight_t)
#' 
#' # Convert importer column codes to new column of full names
#' glimpse(test_data %>% 
#'           add_country_name("importer_iso3c"))
#' 
#' # Provide new column name
#' glimpse(test_data %>% 
#'           add_country_name("importer_iso3c",
#'                             country.col.name = "test_column_name"))
#' 
#' @import dplyr
#' @export

add_country_name <- function(data, 
                             col, 
                             country.col.name = NA){
  if(is.na(country.col.name)) {
    country.col.name <- paste(col, "_name", sep = "")
  }
  
  data <- data %>%
    rename(source_var_col = {{ col }}) %>%
    ungroup() %>%  # Ungroup the data to prevent group-wise issues
    mutate(is_nei = ifelse(source_var_col == "NEI", TRUE, FALSE)) %>%  # Create a column to identify NEI values
    left_join(
      owid_regions %>%  # Join the data to the owid_regions table
        select(code, country_name),
      by = c("source_var_col" = "code")) %>%
    mutate(country_name = case_when(  # Replace NEI and unknown values
      is_nei ~ "Nowhere Else Included",
      source_var_col == "unknown" ~ "Unknown",
      TRUE ~ country_name
    )) %>%
    select(-is_nei)  # Remove the is_nei column
  
  colnames(data)[colnames(data) == "source_var_col"] <- col # Rename the columns
  colnames(data)[colnames(data) == "country_name"] <- country.col.name
  
  return(data)
}

