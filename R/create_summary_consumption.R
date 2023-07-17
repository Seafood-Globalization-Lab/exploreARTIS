#' @export

create_summary_consumption <- function(complete_consumption, per_capita_limit = NA) {
  
  if (!is.na(per_capita_limit)) {
    complete_consumption <- limit_per_capita_consumption(complete_consumption, per_capita_limit)
  }
  
  summary_consumption <- complete_consumption %>%
    group_by(iso3c, hs6, sciname, habitat, method, year, dom_source) %>%
    summarize(consumption_live_t = sum(consumption_live_t, na.rm = TRUE)) %>%
    ungroup() %>%
    pivot_wider(names_from = dom_source, values_from = consumption_live_t, values_fill = 0) %>%
    rename(domestic_consumption_t = domestic,
           foreign_consumption_t = foreign) %>%
    mutate(supply = domestic_consumption_t + foreign_consumption_t)
  
  
  return(summary_consumption)
}
