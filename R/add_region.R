#' @import tidyverse
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
        select(code, region = continent),
      by = c("source_var_col" = "code")) %>%
    mutate(region = case_when(
      source_var_col == "unknown" ~ "Unknown",
      TRUE ~ region
    ))
  
  colnames(data)[colnames(data) == "source_var_col"] <- col
  colnames(data)[colnames(data) == "region"] <- region.col.name
  
  return(data)
}
