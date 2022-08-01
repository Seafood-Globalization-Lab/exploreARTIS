#' @import tidyverse
#' @export

link_transparency_fun <- function(country_to_region = country_to_region, country_iso){
  # Function to control color and transparency of links
  if(sum(is.na(country_iso)) > 0){
    country_to_region %>%
      mutate(., color = case_when(str_detect(exporter_region, "EAsia") ~ e_asia_other,
                                  str_detect(exporter_region, "Euro") ~ euro_other,
                                  str_detect(exporter_region, "LAmer") ~ lat_amer_other,
                                  str_detect(exporter_region, "MidE") ~ mid_east_other,
                                  str_detect(exporter_region, "NAmer") ~ n_amer_other,
                                  str_detect(exporter_region, "SAsia") ~ s_asia_other,
                                  str_detect(exporter_region, "SSAfr") ~ ss_afr_other,
                                  TRUE ~ "white")) %>%
      pull(color)
  }else{
    country_to_region %>%
      mutate(., color = case_when(exporter_region == country_iso | importer_region == country_iso ~ "black",
                                  # Set other exports from regions
                                  importer_region != country_iso & str_detect(exporter_region, "EAsia") ~ e_asia_other,
                                  importer_region != country_iso & str_detect(exporter_region, "Euro") ~ euro_other,
                                  importer_region != country_iso & str_detect(exporter_region, "LAmer") ~ lat_amer_other,
                                  importer_region != country_iso & str_detect(exporter_region, "MidE") ~ mid_east_other,
                                  importer_region != country_iso & str_detect(exporter_region, "NAmer") ~ n_amer_other,
                                  importer_region != country_iso & str_detect(exporter_region, "SAsia") ~ s_asia_other,
                                  importer_region != country_iso & str_detect(exporter_region, "SSAfr") ~ ss_afr_other,
                                  TRUE ~ "white")) %>%
      pull(color)
  }
}
