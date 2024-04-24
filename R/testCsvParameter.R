testCsvParameter <- function(arg_name, arg_value) {
  caller <- sys.call(-1)
  tryCatch({
    if (is.null(arg_value)) {
      message(paste("Warning in", as.character(caller[1]), ":", arg_name, "is NULL. No CSV file will be generated."))
      return()
    }
    
    if (arg_value == FALSE) {
      message(paste("Warning in", as.character(caller[1]), ":", arg_name, "is FALSE. No CSV file will be generated."))
      return()
    }
    
    if (arg_value == TRUE) {
      message(paste("CSVout is TRUE. A CSV file will be generated."))
      return(invisible())  # Ne retourne rien
    }
    
    if (arg_value == "") {
      message(paste("Warning in", as.character(caller[1]), ":", arg_name, "is empty. No CSV file will be generated."))
      invisible(NULL)
    }
    
    if (!is.character(arg_value)) {
      stop(paste("Error in", as.character(caller[1]), ":", arg_name, "must be a character string."))
    }
    
    if (!file.exists(arg_value)) {
      stop(paste("Error in", as.character(caller[1]), ":", "File path for", arg_name, "does not exist."))
    }
    
    # Check if it's a directory path
    if (dir.exists(arg_value)) {
      stop(paste("Error in", as.character(caller[1]), ":", "File path for", arg_name, "is a directory path."))
    }
    
    if (!grepl("\\.csv$", arg_value)) {
      stop(paste("Error in", as.character(caller[1]), ":", "File", arg_name, "does not have the .csv extension."))
    }
    
  }, error = function(e) {
    message(paste("Error in", as.character(caller[1]), ":", e$message))
  })
}
