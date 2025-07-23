#' @title Create a list of prefixes for both CELLAR and FAO repositories. 
#' @description Creates a list of prefixes to be used when building SPARQL queries to retrieve classification tables from CELLAR or FAO.
#' If the global option \code{useLocalDataForVignettes} is set to TRUE (e.g. \code{options(useLocalDataForVignettes = TRUE)}),
#' the function uses local pre-saved metadata instead of querying live endpoints.
#'
#' @param endpoint SPARQL endpoint to query. Must be either \code{"CELLAR"} or \code{"FAO"}.
#' @param prefix Optional. A vector of prefix names to filter. If \code{NULL}, all available prefixes will be returned.
#'
#' @return A character matrix of SPARQL PREFIX declarations.
#'
#' @details
#' The behaviour of this function is contingent on the global option \code{useLocalDataForVignettes}:
#' The default behaviour (when the option is not set, or set to something else than \code{TRUE}), it queries live SPARQL endpoints online.
#' When the option is set to \code{TRUE} via \code{options(useLocalDataForVignettes = TRUE)}, the function returns local (embedded) data instead of querying live SPARQL endpoints.
#' This is useful for building vignettes or offline testing.

#' @import httr


prefixList <- function(endpoint, prefix = NULL) {
  # --- Validate the endpoint argument ---
  endpoint <- toupper(endpoint)
  if (!endpoint %in% c("CELLAR", "FAO")) {
    stop("`endpoint` must be either 'CELLAR' or 'FAO'.")
  }
  
  # --- Define a set of static, common prefixes ---
  prefix_init <- as.matrix(rbind(
    "PREFIX dc: <http://purl.org/dc/elements/1.1/>",
    "PREFIX dct: <http://purl.org/dc/terms/>",
    "PREFIX cb: <http://cbasewrap.ontologycentral.com/vocab#>",
    "PREFIX eli: <http://data.europa.eu/eli/ontology#>",
    "PREFIX euvoc: <http://publications.europa.eu/ontology/euvoc#>",
    "PREFIX owl: <http://www.w3.org/2002/07/owl#>",
    "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>",
    "PREFIX skosxl: <http://www.w3.org/2008/05/skos-xl#>",
    "PREFIX xml: <http://www.w3.org/XML/1998/namespace>",
    "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>",
    "PREFIX skos: <http://www.w3.org/2004/02/skos/core#>",
    "PREFIX xkos: <http://rdf-vocabulary.ddialliance.org/xkos#>",
    "PREFIX is: <http://purl.org/ontology/is/core#>",
    "PREFIX isi: <http://purl.org/ontology/is/inst/>",
    "PREFIX cpc: <https://data.epo.org/linked-data/def/cpc/>",
    "PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>"
  ))
  
  # --- Retrieve dynamic prefixes from the classification list ---
  res <- classificationList(endpoint)
  
  if (!is.matrix(res) || ncol(res) < 3) {
    stop("Unexpected structure returned by classificationList().")
  }
  
  uri <- res[, 3]
  prefix_endpoint <- gsub("\\.", "", res[, 1])  # Remove dots from prefix names
  
  # --- Construct PREFIX declarations from endpoint metadata ---
  dynamic_prefixes <- as.matrix(paste0("PREFIX ", prefix_endpoint, ": <", uri, "/>"))
  
  # --- Combine standard and dynamic prefixes, remove duplicates ---
  prefix_all <- rbind(prefix_init, dynamic_prefixes)
  prefix_all <- prefix_all[!duplicated(prefix_all), , drop = FALSE]
  
  # --- Filter by requested prefixes, if any ---
  if (!is.null(prefix)) {
    valid_prefixes <- prefix[prefix %in% prefix_endpoint]
    if (length(valid_prefixes) == 0) {
      stop("Desired prefixes not found for endpoint ", endpoint, ".")
    }
    
    uri_filtered <- uri[prefix_endpoint %in% valid_prefixes]
    filtered_matrix <- matrix(paste0("PREFIX ", valid_prefixes, ": <", uri_filtered, "/>"))
    
    # Return standard + selected dynamic prefixes
    prefix_all <- rbind(prefix_init, filtered_matrix)
  }
  
  return(prefix_all)
}

