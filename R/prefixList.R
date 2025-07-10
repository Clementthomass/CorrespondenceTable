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


prefixList = function(endpoint, prefix = NULL) {
  #Check correctness of endpoint argument
  endpoint <- toupper(endpoint)
  if (endpoint != "CELLAR" & endpoint != "FAO") {
    stop("Specify the endpoint: CELLAR or FAO.")
  }
  
  prefix_init = as.matrix(rbind(
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
  
  ### Define List
  
  # Check the useLocalDataForVignettes option
  if (getOption("useLocalDataForVignettes", FALSE)) {
    uri = classificationList(endpoint)[, 3]
    prefix_endpoint = classificationList(endpoint)[, 1]
    
  }else{
    uri = classificationList(endpoint)[[1]][, 3]
    prefix_endpoint = classificationList(endpoint)[[1]][, 1]
  }
  prefix_endpoint = gsub("\\.", "", prefix_endpoint)
  # Include the predefined prefixes
  prefix_all = as.matrix(paste0("PREFIX ", prefix_endpoint, ": <", uri, "/>"))
  prefix_all = rbind(prefix_init, prefix_all)
  # remove duplicates
  prefix_all = prefix_all[!duplicated(prefix_all)]
  
  # Check if desired prefixes are available for the given endpoint
  if (!is.null(prefix)) {
    # Check if the desired prefixes are available for the given endpoint
    valid_prefixes = prefix[prefix %in% prefix_endpoint]
    if (length(valid_prefixes) > 0) {
      # Find the URIs corresponding to the desired prefixes
      uri_for_prefix <- uri[prefix_endpoint %in% valid_prefixes]
      
      # Construct the PREFIX statements for the desired prefixes
      prefix_selected <- matrix(paste0("PREFIX ", valid_prefixes, ": <", uri_for_prefix, "/>"))
      prefix_all <- rbind(prefix_init, prefix_selected)
    } else {
      stop("Desired prefixes not found.")
    }
  }
  
  return(prefix_all)
}
