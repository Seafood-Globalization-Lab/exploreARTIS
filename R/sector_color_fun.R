#' @import tidyverse
#' @export

sector_color_fun <- function(country_to_region = country_to_region, 
                             country_iso = NA, plot_region = plot_region, region_colors = region_colors){
  # Function to set sector colors
  # use str_detect in case sector abbreviations change slightly
  
  sectors <- union(country_to_region[[1]], country_to_region[[2]])
  
  if(plot_region == TRUE){
    if("Other" %in% sectors){
      color_vec <- c(region_colors, "grey50")
    }else{
      color_vec <- region_colors
    }
    
  }else{
    if(sum(is.na(country_iso)) > 0){
      color_vec <- data.frame(sector_name = sectors) %>%
        mutate(color = case_when(str_detect(sector_name, "EAsia") ~ e_asia,
                                 str_detect(sector_name, "Euro") ~ euro,
                                 str_detect(sector_name, "LAmer") ~ lat_amer,
                                 str_detect(sector_name, "MidE") ~ mid_east,
                                 str_detect(sector_name, "NAmer") ~ n_amer,
                                 str_detect(sector_name, "SAsia") ~ s_asia,
                                 str_detect(sector_name, "SSAfr") ~ ss_afr,
                                 TRUE ~ "white")) %>%
        pull(color)
    }else{
      color_vec <- data.frame(sector_name = sectors) %>%
        mutate(color = case_when(sector_name %in% country_iso ~ "black",
                                 str_detect(sector_name, "EAsia") ~ e_asia,
                                 str_detect(sector_name, "Euro") ~ euro,
                                 str_detect(sector_name, "LAmer") ~ lat_amer,
                                 str_detect(sector_name, "MidE") ~ mid_east,
                                 str_detect(sector_name, "NAmer") ~ n_amer,
                                 str_detect(sector_name, "SAsia") ~ s_asia,
                                 str_detect(sector_name, "SSAfr") ~ ss_afr,
                                 TRUE ~ "white")) %>%
        pull(color)
    }
  }
  names(color_vec) <- sectors
  return(color_vec)
}
