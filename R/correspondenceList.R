
## To retrieve a list of classification tables in CELLAR/FAO or both.
correspondenceList = function(endpoint) {
  
  if (endpoint == "ALL") {
    cycle = 1:2
  }
  if (endpoint == "CELLAR") {
    cycle = 1
  }
  if (endpoint == "FAO") {
    cycle = 2
  }
  
  data = list()
  
  for (j in cycle) {
    e = c("CELLAR", "FAO")[j]
  
    ### Define endpoint
    if (e == "CELLAR") {
      source = "http://publications.europa.eu/webapi/rdf/sparql"
      sep = "_"
      rm  = 1:14
    }
    if (e == "FAO") {
      source = "https://stats.fao.org/caliper/sparql/AllVocs"
      sep = "-"
      rm  = 1:7
    }
    
    ## Create Prefixes list 
    prefixlist = prefixList(e)
    prefixlist = as.character(paste(prefixlist, collapse = "\n"))
    
    prefixes_loop = unlist(lapply(strsplit(as.character(prefixList(e)), " "), function(x) x[2]))
    prefixes_loop = prefixes_loop[-rm]
  
    data_t = list()
  
    for (i in 1:length(prefixes_loop)){
    prefix = prefixes_loop[i]
    
    SPARQL.query = paste0(prefixlist, "
       SELECT ?ID_table ?A ?B ?Table ?URL 

       WHERE {
             ?s a xkos:Correspondence ;
             skos:prefLabel ?Label .
    
            BIND (STR(?s) AS ?URL)
            BIND (STR(?Label) AS ?Table)
  
            BIND (STRAFTER(STR(?s), STR(", prefix, ")) AS ?ID_table)
   		      BIND (STRAFTER(STR(?ID_table), '", sep, "') AS ?B)
  		      BIND (STRBEFORE(STR(?ID_table), '", sep, "') AS ?A)  
  
            FILTER (STRLEN(?ID_table) != 0) 
        }
      ")
    
    response = POST(url = source, accept("text/csv"), body = list(query = SPARQL.query), encode = "form")
    data_t[[i]] = data.frame(content(response))
    
    if (nrow(data_t[[i]]) == 0){
      data_t[[i]] = cbind(prefix = character(), data_t[[i]])
    } else {
      data_t[[i]] = cbind(prefix = rep(gsub(":","",prefix), nrow(data_t[[i]])), data_t[[i]])
    }
      
    #if (nrow(data_t[[i]]) > 0){
    #  data_t[[i]]$prefix = gsub(":","",prefix)
    #  data_t[[i]] = data_t[[i]][,c(6, 1:5)]
    #  names(data_t)[2] = "ID_table"
    #}
    names(data_t)[i] = prefix
    }
    
    data[[j]] = data_t
    names(data)[j] = c("CELLAR", "FAO")[j]
  }
    
  return(data)
  
}

