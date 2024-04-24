check_n_columns <- function(df,source, num_columns) {
  tryCatch({
    if (ncol(df) == num_columns) {
     # print(paste("The dataframe has exactly", num_columns, "columns."))
    } else {
      stop(paste("The data", source, " does not have exactly", num_columns, "columns."))
    }
  }, error = function(e) {
    print(paste("Error:", e))
  })
}