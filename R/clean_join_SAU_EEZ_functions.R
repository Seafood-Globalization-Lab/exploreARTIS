
standardize_sau_eez <- function(data, col_iso3, col_country_name) {
  
  cleaned_sau <- data %>%
    mutate(artis_iso3 = case_when(
      # United States' territories----------------------------------------------
      .data[[col_iso3]] == "ASM" ~ "USA", # American Samoa
      .data[[col_iso3]] == "GUM" ~ "USA", # Guam
      .data[[col_iso3]] == "MNP" ~ "USA", # New Marianas Islands
      .data[[col_iso3]] == "PRI" ~ "USA", # Puerto Rico
      .data[[col_iso3]] == "VIR" ~ "USA", # Virgin Islands
      # Great Britain's territories---------------------------------------------
      .data[[col_iso3]] == "AIA" ~ "GBR", # Anguilla
      .data[[col_iso3]] == "BMU" ~ "GBR", # Bermuda
      .data[[col_iso3]] == "IOT" ~ "GBR", # British Indian Ocean Territory
      .data[[col_iso3]] == "VGB" ~ "GBR", # British Virgin Islands
      .data[[col_iso3]] == "CYM" ~ "GBR", # Cayman Islands
      .data[[col_iso3]] == "GIB" ~ "GBR", # Gibraltar
      .data[[col_iso3]] == "PCN" ~ "GBR", # Pitcairn Islands
      .data[[col_iso3]] == "SHN" ~ "GBR", # St Helena
      .data[[col_iso3]] == "TCA" ~ "GBR", # Turks and Caicos
      .data[[col_iso3]] == "FLK" ~ "GBR", # Falkland Islands
      .data[[col_iso3]] == "IMN" ~ "GBR", # Isle of Man
      .data[[col_country_name]] == "Channel Islands" ~ "GBR", # Channel Islands does not have an ISO3 code
      .data[[col_iso3]] == "SGS" ~ "GBR", # South Georgia & South Sandwich Islands
      # France's territories----------------------------------------------------
      .data[[col_iso3]] == "PYF" ~ "FRA", # French Polynesia
      .data[[col_iso3]] == "MYT" ~ "FRA", # Mayotte 
      .data[[col_iso3]] == "NCL" ~ "FRA", # New Caledonia
      .data[[col_iso3]] == "SPM" ~ "FRA", # St. Pierre & Miquelon
      .data[[col_iso3]] == "WLF" ~ "FRA", # Wallis and Futuna
      .data[[col_iso3]] == "GUF" ~ "FRA", # French Guiana
      .data[[col_iso3]] == "GLP" ~ "FRA", # Guadeloupe
      .data[[col_iso3]] == "MTQ" ~ "FRA", # Martinique
      .data[[col_iso3]] == "MCO" ~ "FRA", # Monaco
      .data[[col_iso3]] == "REU" ~ "FRA", # Reunion
      .data[[col_iso3]] == "MAF" ~ "FRA", # Saint Martin
      .data[[col_iso3]] == "BLM" ~ "FRA", # Saint Barthelemy
      .data[[col_iso3]] == "ATF" ~ "FRA", # French Southern and Antartic Lands
      # China's territories-----------------------------------------------------
      .data[[col_iso3]] == "HKG" ~ "CHN", # Hong Kong -> China
      .data[[col_iso3]] == "MAC" ~ "CHN", # Macao -> China
      # Netherlands' territories------------------------------------------------
      .data[[col_iso3]] == "ABW" ~ "NLD", # Aruba
      .data[[col_iso3]] == "ANT" ~ "NLD", # Netherlands Antilles
      .data[[col_iso3]] == "BES" ~ "NLD", # Bonaire, Sint Eustatius and Saba
      .data[[col_iso3]] == "SXM" ~ "NLD", # Sint Maarten
      .data[[col_iso3]] == "CUW" ~ "NLD", # Curacao
      # New Zealand's territories-----------------------------------------------
      .data[[col_iso3]] == "COK" ~ "NZL", # Cook Islands
      .data[[col_iso3]] == "NIU" ~ "NZL", # Niue
      .data[[col_iso3]] == "TKL" ~ "NZL", # Tokelau
      # Australia's territories-------------------------------------------------
      .data[[col_iso3]] == "NFK" ~ "AUS", # Norfolk Island
      .data[[col_iso3]] == "CXR" ~ "AUS", # Christmas Island
      .data[[col_iso3]] == "CCK" ~ "AUS", # Cocos (keeling) Islands
      .data[[col_iso3]] == "HMD" ~ "AUS", # Heard & McDonald Islands
      # Denmark's territories---------------------------------------------------
      .data[[col_iso3]] == "GRL" ~ "DNK", # Greenland
      .data[[col_iso3]] == "FRO" ~ "DNK", # Faroe Islands
      # Tanzania's territories--------------------------------------------------
      .data[[col_iso3]] == "EAZ" ~ "TZA", # Zanzibar
      # Norway's territories--------------------------------------------------
      .data[[col_iso3]] == "SJM" ~ "NOR", # Svalbard & Jan Mayen
      .data[[col_iso3]] == "BVT" ~ "NOR", # Bouvet Island
      TRUE ~ .data[[col_iso3]]
    )) %>%
    # Cleaning country names------------------------------------------------------
  # Taiwan: Coded as Other Asia, nes in BACI data
  mutate(artis_country_name = case_when(
    .data[[col_country_name]] == "Other Asia, nes" ~ "Taiwan Province of China",
    TRUE ~ .data[[col_country_name]]
  )) %>%
    mutate(artis_iso3 = case_when(
      artis_country_name == "Taiwan Province of China" ~ "TWN",
      TRUE ~ artis_iso3
    )) %>%
    # Luxembourg gets grouped with Belgium
    mutate(artis_iso3 = case_when(
      artis_iso3 == "LUX" ~ "BEL",
      TRUE ~ artis_iso3
    )) %>%
    # Countries that do not appear in production----------------------------------
  # These are countries that are not territories either
  # Based on previous code assign countries that do not appear in production as Other nei
  mutate(artis_iso3 = case_when(
    artis_iso3 == "SMR" ~ "NEI", # San Marino
    artis_iso3 == "AND" ~ "NEI", # Andorra
    artis_country_name == "US Misc. Pacific Isds" ~ "NEI", # US Misc Pacific Islands
    TRUE ~ artis_iso3
  )) %>%
    mutate(artis_country_name = case_when(
      artis_iso3 == "NEI" ~ "Other nei",
      artis_iso3 == "SCG" ~ "Serbia and Montenegro",
      TRUE ~ artis_country_name
    )) %>%
    # Correcting iso3 letter codes
    mutate(artis_iso3 = case_when(
      artis_country_name == "Other nei" ~ "NEI",
      TRUE ~ artis_iso3
    )) %>%
    # Automated iso3 letter and numeric codes
    mutate(artis_country_name = case_when(
      artis_iso3 != "NEI" & artis_iso3 != "SCG" ~ as.character(countrycode(artis_iso3, origin = "iso3c", destination = "country.name")),
      TRUE ~ artis_country_name
    ))
  
  return(cleaned_sau)
}
