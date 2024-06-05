#' @import tidyverse
#' @export
add_country_name <- function(data, col, country.col.name = NA){
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