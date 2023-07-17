#' @export

limit_per_capita_consumption <- function(complete_consumption, per_capita_limit = 100) {
  
  df <- complete_consumption %>%
    group_by(iso3c, year) %>%
    summarize(consumption_live_t = sum(consumption_live_t, na.rm = TRUE),
              pop = mean(pop, na.rm = TRUE)) %>%
    ungroup() %>%
    # consumption from live weight tonnes to kg
    mutate(consumption_live_kg = 1000 * consumption_live_t) %>%
    mutate(per_capita_consumption = 1000 * consumption_live_kg / pop)
  
  outliers <- df %>%
    filter(per_capita_consumption > per_capita_limit)
  
  non_outliers <- df %>%
    filter(per_capita_consumption <= per_capita_limit) %>%
    mutate(corrected_consumption_t = consumption_live_t)
  
  corrected_outliers <- outliers %>%
    # 100 kg per capita * (1 tonne / 1000 kg)
    mutate(corrected_consumption_t = per_capita_limit * pop / 1000) %>%
    select(iso3c, year, corrected_consumption_t)
  
  new_per_capita_consumption <- non_outliers %>%
    bind_rows(corrected_outliers) %>%
    select(iso3c, year, corrected_consumption_t) %>%
    distinct()
  
  corrected_consumption <- complete_consumption %>%
    group_by(iso3c, year) %>%
    mutate(total_consumption_t = sum(consumption_live_t, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(prop_flow = consumption_live_t / total_consumption_t) %>%
    left_join(
      new_per_capita_consumption,
      by = c("iso3c", "year")
    ) %>%
    mutate(new_consumption_live_t = prop_flow * corrected_consumption_t) %>%
    select(iso3c, hs6, sciname, habitat, method, year, source_country_iso3c, dom_source, hs_version, pop, new_consumption_live_t) %>%
    rename(consumption_live_t = new_consumption_live_t)
  
  test <- corrected_consumption %>%
    group_by(iso3c, year) %>%
    summarize(consumption_live_t = sum(consumption_live_t, na.rm = TRUE),
              pop = mean(pop)) %>%
    ungroup() %>%
    mutate(per_capita_consumption = consumption_live_t / pop) %>%
    filter(per_capita_consumption > per_capita_limit)
  
  if (nrow(test) > 0) {
    warning("ERROR: consumption still exceeds per capita limit")
  }
    
  return(corrected_consumption)
}
