#' @import tidyverse
#' @export

link_transparency_fun <- function(country_to_region = country_to_region, country_iso, trans_value){
  # Function to control color and transparency of links
  
  e_asia_other <- paste(e_asia, trans_value, sep = "")
  s_asia_other <- paste(s_asia, trans_value, sep = "")
  lat_amer_other <- paste(lat_amer, trans_value, sep = "")
  n_amer_other <- paste(n_amer, trans_value, sep = "")
  mid_east_other <- paste(mid_east, trans_value, sep = "")
  ss_afr_other <- paste(ss_afr, trans_value, sep = "")
  euro_other <- paste(euro, trans_value, sep = "")
  
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
