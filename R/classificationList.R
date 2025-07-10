#' @title Retrieve lists of classification tables from the CELLAR and FAO repositories
#' @description Retrieve a list of classification tables from the CELLAR and FAO repositories.
#' @param endpoint One of "CELLAR", "FAO", or "ALL" (default).
#' @param showQuery Logical. If TRUE, returns the SPARQL query along with the data.
#' @details
#' The behaviour of this function is contingent on the global option \code{useLocalDataForVignettes}:
#' The default behaviour (when the option is not set, or set to something else than \code{TRUE}), it queries live SPARQL endpoints online.
#' When the option is set to \code{TRUE} via \code{options(useLocalDataForVignettes = TRUE)}, the function returns local (embedded) data instead of querying live SPARQL endpoints.
#' This is useful for building vignettes or offline testing.
#'
#' @return A data frame (if endpoint is "CELLAR" or "FAO"), or a named list of two data frames (if endpoint is "ALL").
#'
#' @import httr
#' @import jsonlite
#' @export


classificationList <- function(endpoint = "ALL", showQuery = FALSE) {
  endpoint <- toupper(endpoint)
  if (!(endpoint %in% c("ALL", "FAO", "CELLAR"))) {
    stop(simpleError(paste("The endpoint value:", endpoint, "is not accepted")))
  }
  
  if (endpoint == "ALL") {
    return(list(
      CELLAR = classificationList("CELLAR", showQuery = showQuery),
      FAO = classificationList("FAO", showQuery = showQuery)
    ))
  }
  
  # Use static files when requested (for vignettes, etc.)
  if (getOption("useLocalDataForVignettes", FALSE)) {
    path <- system.file("extdata", paste0("classificationlList_", endpoint, ".csv"), package = "correspondenceTables")
    if (file.exists(path)) {
      return(read.csv(path))
    } else {
      stop(paste("Local file for", endpoint, "is missing."))
    }
  }
  
  # SPARQL setup
  SPARQL.query <- ""
  endpoint_url <- ""
  
  tryCatch({
    config <- fromJSON("https://raw.githubusercontent.com/eurostat/correspondenceTables/refs/heads/main/inst/extdata/endpoint_source_config.json")
    
    if (endpoint == "CELLAR") {
      endpoint_url <- config$CELLAR
      SPARQL.query <- "
        SELECT DISTINCT ?s ?Title
        WHERE { ?s a skos:ConceptScheme ;
                skos:prefLabel ?Title ;
                ?p <http://publications.europa.eu/resource/authority/corporate-body/ESTAT> 
                 FILTER (LANG(?Title) = 'en')}
        ORDER BY ?Title
      "
    } else if (endpoint == "FAO") {
      endpoint_url <- config$FAO
      SPARQL.query <- "
        PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
        PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
        PREFIX skos: <http://www.w3.org/2004/02/skos/core#>

        SELECT ?scheme ?notation ?label_en WHERE {
          ?scheme rdf:type skos:ConceptScheme .
          ?scheme skos:notation ?notation .
          ?scheme skos:prefLabel ?label_en . FILTER(lang(?label_en)='en')
        }
        ORDER BY ASC(?notation)
      "
    }
    
    response <- POST(url = endpoint_url, httr::accept("text/csv"), body = list(query = SPARQL.query), encode = "form")
    df <- read.csv(text = content(response, "text"), sep = ",")
    
    if (endpoint == "CELLAR") {
      str_dt <- t(sapply(df[, 1], function(x) unlist(strsplit(as.character(x), "/+"))))
      uri <- paste0(str_dt[, 1], "/", "/", str_dt[, 2], "/", str_dt[, 3], "/", str_dt[, 4])
      prefix <- gsub("\\.", "", str_dt[, 4])
      conceptscheme <- str_dt[, 5]
      title <- df[, 2]
    } else if (endpoint == "FAO") {
      str_dt <- strsplit(df[, 1], "/")
      mat_str_dt <- suppressWarnings(do.call(rbind, str_dt))
      df_str_dt <- as.data.frame(mat_str_dt)
      prefix <- df[, 2]
      conceptscheme <- paste0(df_str_dt[, 5], df_str_dt[, 6])
      
      # NOTE: workaround for FAO URIs
      # FAO endpoint sometimes returns incomplete URIs, so we use the local fallback as backup.
      # (This behavior was introduced by Bienvenu, Loïc)
      # See review request 0.10.22/015
      uri <- read.csv(system.file("extdata", "classificationlList_FAO.csv", package = "correspondenceTables"))[, 3]
      title <- df[, 3]
    }
    
    result <- cbind(prefix, conceptscheme, uri, title)
    colnames(result) <- c("Prefix", "ConceptScheme", "URI", "Title")
    rownames(result) <- 1:nrow(result)
    
    if (showQuery) {
      return(list("SPARQL.query" = SPARQL.query, "ClassificationList" = result))
    } else {
      return(result)
    }
    
  }, error = function(e) {
    message("The following SPARQL code was used in the call:\n", SPARQL.query)
    message("The above SPARQL call to ", endpoint_url, " generated the following error message:\n", conditionMessage(e))
    stop(simpleError(paste("Error in function ClassificationList(", endpoint, "), Endpoint", endpoint, "is not available or is returning unexpected data")))
  })
}
