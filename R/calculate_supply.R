#' @import tidyverse
#' @export

calculate_supply <- function(artis_data, production_data){
  exports <- artis_data %>%
    group_by(year, exporter_iso3c, dom_source, sciname, method, environment) %>%
    summarise(live_weight_t = sum(live_weight_t)) %>%
    mutate(dom_source = str_replace(dom_source, " ", "_")) %>%
    pivot_wider(names_from = "dom_source", values_from = "live_weight_t")
  
  imports <- artis_data %>%
    group_by(year, importer_iso3c, sciname, method, environment) %>%
    summarise(imports_live_weight_t = sum(live_weight_t))
  
  supply <- exports %>%
    full_join(imports, by = c("year", "exporter_iso3c" = "importer_iso3c", 
                              "sciname", "method", "environment")) %>%
    full_join(production_data %>% 
                rename(production_t = live_weight_t), by = c("year", "exporter_iso3c" = "iso3c", 
                           "sciname", "method" = "prod_method", "environment")) 
  
  supply$foreign_export[is.na(supply$foreign_export)] <- 0
  supply$domestic_export[is.na(supply$domestic_export)] <- 0
  supply$error_export[is.na(supply$error_export)] <- 0
  supply$imports_live_weight_t[is.na(supply$imports_live_weight_t)] <- 0
  supply$production_t[is.na(supply$production_t)] <- 0
  
  supply <- supply %>%
    mutate(supply = production_t + imports_live_weight_t - domestic_export - foreign_export - error_export,
           supply_no_error = production_t + imports_live_weight_t - domestic_export - foreign_export, 
           supply_domestic = production_t - domestic_export,
           supply_foreign = imports_live_weight_t - foreign_export) %>%
    rename("iso3c" = "exporter_iso3c") %>%
    ungroup() %>%
    mutate(supply_no_error = replace_na(supply_no_error, 0),
           supply_domestic = replace_na(supply_domestic, 0),
           supply_foreign = replace_na(supply_foreign, 0))
  
  return(supply)
}