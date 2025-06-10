#' List file metadata from a KNB resource map
#'
#' Connects to the KNB (Knowledge Network for Biocomplexity) DataONE node and retrieves
#' metadata about all files associated with a given resource map.
#'
#' @param resource_map_id Character. KNB resource map identifier (e.g., "resource_map_urn:uuid:...").
#'
#' @return A data frame with metadata for each file, including:
#'   - identifier: DataONE object identifier
#'   - fileName: File name (if available)
#'   - formatType: Format type (e.g., METADATA, DATA)
#'   - formatId: MIME type (e.g., text/csv, application/zip)
#'   - title: File title from metadata
#'   - size: File size in bytes
#'
#' @importFrom dataone CNode getMNode query
#' @export
list_knb_files <- function(resource_map_id) {
  # Install 'dataone' package if not already installed
  if (!requireNamespace("dataone", quietly = TRUE)) {
    install.packages("dataone")
  }
  library(dataone)

  # Connect to the production Coordinating Node and Member Node (KNB)
  cn <- CNode("PROD")
  mn <- getMNode(cn, "urn:node:KNB")

  # Define Solr query to fetch all files under the resource map
  query_list <- list(
    q = paste0('resourceMap:"', resource_map_id, '"'),
    fl = "identifier,fileName,formatType,formatId,title,size",
    rows = "200"  # Adjust if you expect more than 200 files
  )

  # Run the query on the KNB Member Node
  result <- query(mn, solrQuery = query_list)

  # Parse results into a data frame, filling missing values with NA
  df <- do.call(rbind, lapply(result, function(item) {
    data.frame(
      identifier  = item$identifier  %||% NA,
      fileName    = item$fileName    %||% NA,
      formatType  = item$formatType  %||% NA,
      formatId    = item$formatId    %||% NA,
      title       = item$title       %||% NA,
      size        = as.numeric(item$size) %||% NA,
      stringsAsFactors = FALSE
    )
  }))

  return(df)
}

#' Null-coalescing operator: returns 'a' if not NULL, otherwise 'b'
`%||%` <- function(a, b) if (!is.null(a)) a else b
