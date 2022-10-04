
#' @import tidyverse
#' @import countrycode
#' @export

convert_owid_regions <- function(df) {
  
  df <- df %>%
    # Converting exporting countries to exporting regions
    left_join(
      owid_regions %>%
        select(code, "exporter_region" = "region"),
      by = c("exporter_iso3c" = "code")
    ) %>%
    # Converting importing countries to importing regions
    left_join(
      owid_regions %>%
        select(code, "importer_region" = "region"),
      by = c("importer_iso3c" = "code")
    ) %>%
    # Re-summarize data based on new exporter and importer region classification
    group_by(exporter_region, importer_region, hs6, sciname, habitat, method, hs_version, year) %>%
    summarize(product_weight_t = sum(product_weight_t, na.rm = TRUE),
              live_weight_t = sum(live_weight_t, na.rm = TRUE))
  
  
  return(df)
}