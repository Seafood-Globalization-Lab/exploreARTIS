#' Process KNB Resource Map and Build DuckDB Database
#'
#' Downloads files from a KNB resource map, unzips them, and builds a DuckDB database
#' from CSV and Parquet files. Includes a metadata table describing all ingested tables.
#'
#' @param resource_map_id Character. KNB resource map identifier (e.g., "resource_map_urn:uuid:...").
#' @param duckdb_name Character. Name of the DuckDB database file to create. Default is "ARTIS_SAU.duckdb".
#' @param directory Character. Directory path where files will be downloaded and processed.
#' @param timeout_seconds Numeric. Download timeout in seconds. Default is 600.
#' @param wait_minutes Numeric. Pause after download before DB build (in minutes). Default is 1.
#' @param artis_custom_timeseries Logical. If TRUE, filters ARTIS `trade` and `consumption` tables by HS version and year.
#' @param hs_version_filter Character vector. HS versions to keep (e.g., "HS_2012"). Used only if artis_custom_timeseries = TRUE.
#' @param year_filter Numeric vector. Years to keep (e.g., 2015:2020). Used only if artis_custom_timeseries = TRUE.
#'
#' @return No return value. A DuckDB database is written to disk.
#' @export process_knb_to_duckdb
#'
#' @importFrom dataone CNode getMNode getObject query
#' @importFrom datapack DataPackage
#' @importFrom duckdb duckdb dbConnect dbWriteTable dbDisconnect
#' @importFrom arrow read_parquet
#' @importFrom utils install.packages unzip writeBin download.file
#' @importFrom tools file_ext
process_knb_to_duckdb <- function(resource_map_id,
                                  duckdb_name = "ARTIS_SAU.duckdb",
                                  directory,
                                  timeout_seconds = 600,
                                  wait_minutes = 1,
                                  artis_custom_timeseries = FALSE,
                                  hs_version_filter = NULL,
                                  year_filter = NULL) {
  options(timeout = timeout_seconds)
  download_from_knb(resource_map_id, directory)
  unzip_all_files(directory)
  Sys.sleep(wait_minutes * 60)

  create_duckdb_from_files(
    folder = directory,
    db_path = duckdb_name,
    artis_custom_timeseries = artis_custom_timeseries,
    hs_version_filter = hs_version_filter,
    year_filter = year_filter
  )

  message("DuckDB created at ", duckdb_name)
}

# ---- HELPER FUNCTIONS ----

# Download data objects from a KNB resource map
download_from_knb <- function(resource_map_id, local_path) {
  cn <- CNode("PROD")
  mn <- getMNode(cn, "urn:node:KNB")

  query_list <- list(
    q = paste0('resourceMap:"', resource_map_id, '"'),
    fl = "identifier,fileName,formatType,formatId,title,size",
    rows = "100"
  )

  result <- query(mn, solrQuery = query_list)
  if (!dir.exists(local_path)) dir.create(local_path, recursive = TRUE)

  small_files <- list()
  large_files <- list()

  for (i in seq_along(result)) {
    size_bytes <- as.numeric(result[[i]]$size)
    file_info <- list(
      identifier = result[[i]]$identifier,
      fileName = result[[i]]$fileName,
      size = size_bytes
    )
    if (size_bytes < 50e6) small_files[[length(small_files) + 1]] <- file_info
    else large_files[[length(large_files) + 1]] <- file_info
  }

  for (f in small_files) {
    tryCatch({
      file_raw <- getObject(mn, f$identifier)
      filename <- ifelse(is.null(f$fileName) || f$fileName == "", paste0(f$identifier, ".dat"), f$fileName)
      filepath <- file.path(local_path, filename)
      writeBin(file_raw, filepath)
    }, error = function(e) {
      cat("Failed to download (API):", f$fileName, "Error:", e$message, "\n")
    })
  }

  for (f in large_files) {
    tryCatch({
      url <- paste0("https://knb.ecoinformatics.org/knb/d1/mn/v2/object/", f$identifier)
      filename <- ifelse(is.null(f$fileName) || f$fileName == "", paste0(f$identifier, ".dat"), f$fileName)
      filepath <- file.path(local_path, filename)
      download.file(url, destfile = filepath, mode = "wb", quiet = TRUE)
    }, error = function(e) {
      cat("Failed to download (HTTP):", f$fileName, "Error:", e$message, "\n")
    })
  }
}

# Unzip all .zip files in the directory
unzip_all_files <- function(local_path) {
  zip_files <- list.files(local_path, pattern = "\\.zip$", full.names = TRUE)
  for (zip_file in zip_files) {
    unzip(zip_file, exdir = local_path)
  }
}

# Read data from folder, apply filtering if enabled, and create DuckDB
create_duckdb_from_files <- function(folder,
                                     db_path,
                                     artis_custom_timeseries = FALSE,
                                     hs_version_filter = NULL,
                                     year_filter = NULL) {
  con <- dbConnect(duckdb::duckdb(), dbdir = db_path, read_only = FALSE)

  read_csv_safe <- function(name) {
    path <- file.path(folder, paste0(name, ".csv"))
    if (file.exists(path)) read.csv(path) else NULL
  }

  read_parquet_safe <- function(name) {
    path <- list.files(folder, pattern = paste0(name, ".*\\.parquet$"), full.names = TRUE)
    if (length(path) == 0) return(NULL)
    return(as.data.frame(arrow::read_parquet(path[1])))
  }

  # Always load these small CSVs
  files <- c("baci", "code_max_resolved", "countries", "products", "sciname")
  for (f in files) {
    df <- read_csv_safe(f)
    if (!is.null(df)) dbWriteTable(con, f, df, overwrite = TRUE)
  }

  # Handle large ARTIS files
  consumption_df <- read_parquet_safe("consumption")
  trade_df <- read_parquet_safe("trade")

  # Apply filters only if custom mode is enabled
  if (artis_custom_timeseries) {
    if (!is.null(hs_version_filter)) {
      if (!is.null(consumption_df)) {
        consumption_df <- subset(consumption_df, hs_version %in% hs_version_filter)
      }
      if (!is.null(trade_df)) {
        trade_df <- subset(trade_df, hs_version %in% hs_version_filter)
      }
    }
    if (!is.null(year_filter)) {
      if (!is.null(consumption_df)) {
        consumption_df <- subset(consumption_df, year %in% year_filter)
      }
      if (!is.null(trade_df)) {
        trade_df <- subset(trade_df, year %in% year_filter)
      }
    }
  }

  if (!is.null(consumption_df)) {
    dbWriteTable(con, "consumption", consumption_df, overwrite = TRUE)
  }

  if (!is.null(trade_df)) {
    dbWriteTable(con, "trade", trade_df, overwrite = TRUE)
  }

  # Add metadata table
  metadata <- data.frame(
    table_name = c("trade", "consumption", "baci", "code_max_resolved", "countries", "products", "sciname"),
    table_description = c(
      "ARTIS trade flows",
      "Consumption estimates",
      "BACI bilateral trade",
      "Taxonomic resolution table",
      "Country info",
      "Product info",
      "Scientific names"
    )
  )
  dbWriteTable(con, "metadata", metadata, overwrite = TRUE)

  dbDisconnect(con)
}
