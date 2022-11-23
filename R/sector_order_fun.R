#' @import tidyverse
#' @export

sector_order_fun <- function(country_to_region = country_to_region, 
                             country_iso = NA, plot_region = plot_region){
  # Function to set sector order
  # use str_detect in case sector abbreviations change slightly
  all_sectors <- union(country_to_region[[1]], country_to_region[[2]])
  
  if(plot_region == TRUE){
    if("Other" %in% all_sectors){
      final_order <- c(all_sectors[str_detect(all_sectors, "Asia")],
                       all_sectors[str_detect(all_sectors, "Oceania")],
                       all_sectors[str_detect(all_sectors, "Europe")],
                       all_sectors[str_detect(all_sectors, "Africa")],
                       all_sectors[str_detect(all_sectors, "South America")],
                       all_sectors[str_detect(all_sectors, "North America")],
                       all_sectors[str_detect(all_sectors, "Other")])
    }else{
      final_order <- c(all_sectors[str_detect(all_sectors, "Asia")],
                       all_sectors[str_detect(all_sectors, "Oceania")],
                       all_sectors[str_detect(all_sectors, "Europe")],
                       all_sectors[str_detect(all_sectors, "Africa")],
                       all_sectors[str_detect(all_sectors, "South America")],
                       all_sectors[str_detect(all_sectors, "North America")])
    }
  }else{
    if(sum(is.na(country_iso)) > 0){
      final_order <- c(all_sectors[str_detect(all_sectors, "EAsia")],
                       all_sectors[str_detect(all_sectors, "SAsia")],
                       all_sectors[str_detect(all_sectors, "LAmer")],
                       all_sectors[str_detect(all_sectors, "MidE")],
                       all_sectors[str_detect(all_sectors, "SSAfr")],
                       all_sectors[str_detect(all_sectors, "NAmer")],
                       all_sectors[str_detect(all_sectors, "Euro")])
    }else{
      final_order <- c(country_iso, 
                       all_sectors[str_detect(all_sectors, "EAsia")],
                       all_sectors[str_detect(all_sectors, "SAsia")],
                       all_sectors[str_detect(all_sectors, "LAmer")],
                       all_sectors[str_detect(all_sectors, "MidE")],
                       all_sectors[str_detect(all_sectors, "SSAfr")],
                       all_sectors[str_detect(all_sectors, "NAmer")],
                       all_sectors[str_detect(all_sectors, "Euro")])
    }
  }
  
  return(final_order)
}
