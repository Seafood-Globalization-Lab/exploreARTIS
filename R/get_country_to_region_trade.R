#' @import tidyverse
#' @export

get_country_to_region_trade <- function(S_net, quantity) {
  S_net %>%
    group_by(exporter_region, importer_region) %>%
    summarise(total_quantity = sum(.data[[quantity]], na.rm = TRUE)) 
}
