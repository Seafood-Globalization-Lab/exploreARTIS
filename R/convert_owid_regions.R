#' Transform ARTIS from country to regional data
#' 
#' `convert_owid_regions( )` takes an ARTIS dataframe and summarizes export and import countries as regions.
#' @param df dataframe. An ARTIS dataframe (needs to include source_country_iso3c, exporter_iso3c, importer_iso3c)
#' @examples
#' #sumarize by product_weight_t and live_weight_t by exporter_region, importer_region, hs6, sciname, habitat, method, hs_version
#' glimpse(mini_artis %>% convert_owid_regions())
#' 
#' @import dplyr
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
    # Correct NA from exporter and importer region
    mutate(
      exporter_region = case_when(
        is.na(exporter_region) ~ "Other",
        TRUE ~ exporter_region),
      importer_region = case_when(
        is.na(importer_region) ~ "Other",
        TRUE ~ importer_region)
      ) %>%
    # Re-summarize data based on new exporter and importer region classification
    group_by(exporter_region, importer_region, hs6, sciname, habitat, method, hs_version, year) %>%
    summarize(product_weight_t = sum(product_weight_t, na.rm = TRUE),
              live_weight_t = sum(live_weight_t, na.rm = TRUE))
  
  
  return(df)
}