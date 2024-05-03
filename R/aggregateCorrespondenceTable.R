#' @title aggregateCorrespondenceTable aggregates correspondence tables to higher hierarchical levels
#' @description The `aggregateCorrespondenceTable` function is designed to aggregate correspondence tables between two hierarchical classifications A and B to higher hierarchical levels. This is particularly useful when correspondence information is needed at levels other than the most granular level. The function provides a 'mechanically defined' aggregation, offering users candidate aggregations for subsequent analysis by statistical classification experts.
#' @param AB a mandatory argument containing a correspondence table data frame with columns "Acode" and "Bcode" representing the correspondence between classifications A and B at the most granular level. This argument is mandatory
#' @param A a path to a CSV file containing source classification data with an Acode ALevel,ASuperior column. This argument is mandatory 
#' @param B a path to a CSV file containing target classification data with a Bcode Blevel BSuperior column. This argument is mandatory
#'
#' @param CSVout a character string providing the path where the aggregated correspondence table CSV file should be saved. If NULL, no CSV file is generated.
#'
#' @return A data frame representing the aggregated correspondence table.
#'
#' @export
#' @examples 
#' # Use data from the folder extdata
#' AB <- (system.file("extdata", "ab_data.csv", package = "correspondenceTables"))
#' A <- (system.file("extdata", "a_data.csv", package = "correspondenceTables"))
#' B <- (system.file("extdata", "b_data.csv", package = "correspondenceTables"))
#' 
#' 
#' result <- aggregateCorrespondenceTable(AB = AB, A = A, B = B, CSVout = FALSE)
#' print(result)
#'
aggregateCorrespondenceTable <- function(AB, A, B, CSVout = NULL ) {
  
  ab_data <- testInputTable("Correspondence table (AB)", AB)
  a_data <- testInputTable("Source classification (A)", A)
  b_data <- testInputTable("Target classification (B)", B)
  
#Check if required number of columns are present in each input
check_n_columns(ab_data,"Correspondence table (AB)", 2)
check_n_columns(a_data, "Source classification (A)", 3)
check_n_columns(b_data,"Target classification (B)", 3)
  
  # Read the input correspondence table AB
  # ab_data <- read.csv2(AB, header = TRUE, sep =",")
  ColumnNames_ab <- colnames(ab_data)[1:2]
  colnames(ab_data)[1:2] = c("Acode","Bcode")
 
  
  # Find duplicated combinations of Acode and Bcode in AB
  duplicated_rows <- ab_data[duplicated(ab_data[c("Acode", "Bcode")]), c("Acode", "Bcode")]
  
  # Check for duplicate combinations of Acode and Bcode
  if (nrow(duplicated_rows) > 0) {
    print("Duplicate(s) combinations of Acode and Bcode found in the input correspondence table AB :")
    print(duplicated_rows)
    tryCatch(stop("Please remove duplicate(s) combinations of Acode and Bcode from the input correspondence table AB."), error = function(e) {})
  } else {
    # print("No duplicate(s) combinations of Acode and Bcode found in the input correspondence table AB.")
  }
  
  # Check if there are any records left
  if (nrow(ab_data) == 0) {
    tryCatch(stop("No valid records found in the input correspondence table AB."), error = function(e) {})
  }
  
  # Filter rows where Acode or Bcode is missing in the AB data
  missing_code_rows <- ab_data[is.na(ab_data$Acode) | ab_data$Acode == "" | is.na(ab_data$Bcode) | ab_data$Bcode == "", ]
  
  # Display problematic rows
  if (nrow(missing_code_rows) > 0) {
    print(paste("Rows with missing values in the", ColumnNames_ab[1], "or", ColumnNames_ab[2], "column of the AB data:"))
    print(missing_code_rows)
    cat("\n")
  }
  
  
  
  ######## 
  ####Read the source classification table A
 
  ColumnNames_a <- colnames(a_data)[1:3]
  colnames(a_data)[1:3] = c("Acode","Alevel","Asuperior")
  
  # Check if there are any records left in table A
  tryCatch({
    if (nrow(a_data) == 0) {
      stop("No valid records found in the input correspondence table A.")
    }
  }, error = function(e) {
    cat("An error occurred while processing input correspondence table A:\n")
    cat(e$message, "\n")
  })
  
  
  # Filter rows where there are NA or empty values in the Alevel column
  problematic_rows <- a_data[is.na(a_data$Alevel) | a_data$Alevel == "", ]
  
  # Display problematic rows
  if (nrow(problematic_rows) > 0) {
    print(paste("Rows with missing or empty values in the Alevel column:", ColumnNames_a[2]))
    print(problematic_rows)
    cat("\n")
  }
  
  # Check for duplicate Acode values in table A
  Aduplicated_rows <- a_data[duplicated(a_data$Acode), "Acode"]
  if (length(Aduplicated_rows) > 0) {
    print(paste("Duplicate(s) value(s) of Acode", ColumnNames_a[1], "found in the input table A:"))
    print(Aduplicated_rows)
    tryCatch(stop(paste("Please remove duplicate(s) values of Acode,", ColumnNames_a[1], "in the input table A.")), error = function(e) {})
  } else {
    # print("No duplicate(s) value(s) of Acode in the input table A.")
  }
  
  
  # Identify rows with text in Asuperior for level 1 records
  a_level_1_with_text <- a_data[a_data$Alevel == 1 & !is.na(a_data$Asuperior) & a_data$Asuperior != "", ]
  
  # Display rows with text in Asuperior for level 1 records
  if (nrow(a_level_1_with_text) > 0) {
    print(paste("The following records at level 1 have text in the Asuperior column:", ColumnNames_a[3]))
    print(a_level_1_with_text)
    cat("\n")
  }
  
  # Check if Asuperior is a character or blank for records at level 1
  tryCatch({
    if (!all((is.character(a_data$Asuperior) & a_data$Alevel != 1) | (is.na(a_data$Asuperior) & a_data$Alevel == 1))) {
      stop(paste("Asuperior column,", ColumnNames_a[3], "in the source classification table A must be blank for records at level 1."))
    }
  }, error = function(e) {})
  
  # Initialize the variable to store the current level for A
  mostGranularA <- max(a_data$Alevel)
  currentLevelA <- mostGranularA
  
  # Loop to check hierarchy at each level for A
  while (currentLevelA >= 2) {
    # Select rows at the current level and the level below
    Ai <- a_data[a_data$Alevel == currentLevelA, ]
    AiMinus1 <- a_data[a_data$Alevel == (currentLevelA - 1), ]
    
    # Check if all values of Asuperior (in Ai) correspond to values of Acode (in AiMinus1)
    error_rows <- which(!(Ai$Asuperior %in% AiMinus1$Acode))
    if (length(error_rows) > 0) {
      cat("Hierarchy error in A_data at level:", currentLevelA, "\n")
      cat("Error occurred in rows:", error_rows, "\n")
      tryCatch(stop("Hierarchy error detected in A_data."), error = function(e) {})
    } 
    
    # Check if all values of Acode (in AiMinus1) correspond to values of Asuperior (in Ai)
    error_rows <- which(!(AiMinus1$Acode %in% Ai$Asuperior))
    if (length(error_rows) > 0) {
      cat("Hierarchy error in A-data at level:", currentLevelA - 1, "\n")
      cat("Error occurred in rows:", error_rows, "\n")
      tryCatch(stop("Hierarchy error detected in A_data."), error = function(e) {})
    }
    
    # Move to the next level
    currentLevelA <- currentLevelA - 1
  }
  
  
  # Read the target classification table B
  ColumnNames_b <- colnames(b_data)[1:3]
  colnames(b_data)[1:3] = c("Bcode","Blevel","Bsuperior")
  
  
  # Check if there are any records left in table B
  if (nrow(b_data) == 0) {
    tryCatch(stop("No valid records found in the input correspondence table B."), error = function(e) {})
  }
  
  # Filter rows where there are NA or empty values in the Blevel column
  problematic_rows <- b_data[is.na(b_data$Blevel) | b_data$Blevel == "", ]
  
  # Display problematic rows
  if (nrow(problematic_rows) > 0) {
    print(paste("Rows with missing or empty values in the Blevel column:", ColumnNames_b[2]))
    print(problematic_rows)
    cat("\n")
  }
  
  # Check for duplicate Bcode values in table B
  Bduplicated_rows <- b_data[duplicated(b_data$Bcode), "Bcode"]
  if (length(Bduplicated_rows) > 0) {
    print(paste("Duplicate(s) value(s) of Bcode", ColumnNames_b[1], "found in the input table B :"))
    print(Bduplicated_rows)
    tryCatch(stop(paste("Please remove duplicate(s) value(s) of Bcode,", ColumnNames_b[1],"in the input table B .")), error = function(e) {})
  } else {
    # print("No duplicate(s) value(s) of Bcode in the input table B .")
  }
  
  
  # Identify rows with text in Bsuperior for level 1 records
  b_level_1_with_text <- b_data[b_data$Blevel == 1 & !is.na(b_data$Bsuperior) & b_data$Bsuperior != "", ]
  
  # Display rows with text in Bsuperior for level 1 records
  if (nrow(b_level_1_with_text) > 0) {
    print(paste("Bsuperior column,", ColumnNames_b[3], "in the target classification table B must be blank for records at level 1."))
    print(b_level_1_with_text)
    cat("\n")
  }
  
  # Check if Bsuperior is a character or blank for records at level 1
  tryCatch({
    if (!all((is.character(b_data$Bsuperior) & b_data$Blevel != 1) | (is.na(b_data$Bsuperior) & b_data$Blevel == 1))) {
      stop(paste("Bsuperior column,", ColumnNames_b[3], "in the source classification table B must contain characters or be blank for records at level 1."))
    }
  }, error = function(e) {})
  
  # Initialize the variable to store the current level
  mostGranularB <- max(b_data$Blevel)
  currentLevelB <- mostGranularB
  
  while (currentLevelB >= 2) {
    # Select rows at the current level and the level below
    Bi <- b_data[b_data$Blevel == currentLevelB, ]
    BiMinus1 <- b_data[b_data$Blevel == (currentLevelB - 1), ]
    
    # Check if all values of Bsuperior (in Bi) correspond to values of Bcode (in BiMinus1)
    error_rows <- which(!(Bi$Bsuperior %in% BiMinus1$Bcode))
    if (length(error_rows) > 0) {
      cat("Hierarchy error in B_data at level:", currentLevelB, "\n")
      cat("Error occurred in rows:", error_rows, "\n")
      tryCatch(stop("Hierarchy error detected in B_data."), error = function(e) {})
    } 
    
    # Check if all values of Bcode (in BiMinus1) correspond to values of Bsuperior (in Bi)
    error_rows <- which(!(BiMinus1$Bcode %in% Bi$Bsuperior))
    if (length(error_rows) > 0) {
      cat("Hierarchy error in B_data at level:", currentLevelB - 1, "\n")
      cat("Error occurred in rows:", error_rows, "\n")
      tryCatch(stop("Hierarchy error detected in B_data."), error = function(e) {})
    }
    
    # Move to the next level
    currentLevelB <- currentLevelB - 1
  }
  
  # Uniqueness Check if Acode and Bcode in AB exist in A and B respectively
  if (!all(ab_data$Acode %in% a_data$Acode)) {
    tryCatch(stop("Acode in the input correspondence table does not exist in source or target classification table."), error = function(e) {})
  } else if (!all(ab_data$Bcode %in% b_data$Bcode)){
    tryCatch(stop("Bcode in the input correspondence table does not exist in source or target classification table."), error = function(e) {})
  }
  
  
  
  
  ###3.4 Correct and complete correspondences 
  
  ###add additional the column because here you just add the code you need all the column
  AmostGranular <- subset(a_data, Alevel == max(Alevel), select = c(Acode, Asuperior))
  BmostGranular <- subset(b_data, Blevel == max(Blevel), select = c(Bcode,Bsuperior))
  
  AB_mostGranular <- merge(AmostGranular, BmostGranular, by.x = "Acode", by.y = "Bcode")
  
  if (!(all(ab_data$Acode %in% AmostGranular$Acode))) {
    stop("Acode in the input correspondence table does not exist in source or target classification table.")
  }
  if (!(all(ab_data$Bcode %in% BmostGranular$Bcode))) {
    stop("Bcode in the input correspondence table does not exist in source or target classification table.")
  }
  
  if (!(all(AB_mostGranular$Acode %in% ab_data$Acode))) {
    stop("Acode in the most granular correspondence table does not exist in the input correspondence table.")
  }
  if (!(all(AB_mostGranular$Bcode %in% ab_data$Bcode))) {
    stop("Bcode in the most granular correspondence table does not exist in the input correspondence table.")
  }
  ########## 4.1 Creation of the table and merge it. 
  
  # Create an empty list to store the levels
  A_levels <- list()
  
  # Loop through each level and subset the data
  for (i in 1:mostGranularA) {
    level_data <- subset(a_data, a_data$Alevel == i, select = c(Acode, Asuperior))
    A_levels[[i]] <- level_data
  }
  
  # Create an empty data frame to store the final result for A
  resultA <- data.frame()
  
  # Initialize the result with the most granular level for A
  resultA <- A_levels[[mostGranularA]]
  
  # Merge the tables hierarchically starting from the second most granular level for A
  for (i in (mostGranularA - 1):1) {
    level_data <- A_levels[[i]]
    
    # Merge with the result using Asuperior and Acode columns
    resultA <- merge(level_data, resultA, by.x = "Acode", by.y = "Asuperior", all.x = TRUE, all.y = TRUE, suffixes = c(paste0(".x", i), paste0(".y", i)))
    
    # Rename columns to reflect the hierarchy for A
    colnames(resultA)[colnames(resultA) == paste0("Acode.x", i)] <- paste0("Acode", i)
    colnames(resultA)[colnames(resultA) == paste0("Acode.y", i)] <- paste0("Acode", (i + 1))
  }
  
  # Result will contain the final aggregated correspondence table with hierarchical code columns for A
  resultA$test <- resultA[[paste0("Acode", mostGranularA)]]
  resultA$Asuperior <- NULL
  
  # Determine the most granular level dynamically
  mostGranularB <- max(b_data$Blevel)
  
  # Create an empty list to store the levels
  B_levels <- list()
  
  # Loop through each level and subset the data
  for (i in 1:mostGranularB) {
    level_data <- subset(b_data, b_data$Blevel == i, select = c(Bcode, Bsuperior))
    B_levels[[i]] <- level_data
  }
  
  # Create an empty data frame to store the final result for B
  resultB <- data.frame()
  
  # Initialize the result with the most granular level for B
  resultB <- B_levels[[mostGranularB]]
  
  # Merge the tables hierarchically starting from the second most granular level for B
  for (i in (mostGranularB - 1):1) {
    level_data_B <- B_levels[[i]]
    
    # Merge with the result using Bsuperior and Bcode columns
    resultB <- merge(level_data_B, resultB, by.x = "Bcode", by.y = "Bsuperior", all.x = TRUE, all.y = TRUE, suffixes = c(paste0(".x", i), paste0(".y", i)))
    
    # Rename columns to reflect the hierarchy for B
    colnames(resultB)[colnames(resultB) == paste0("Bcode.x", i)] <- paste0("Bcode", i)
    colnames(resultB)[colnames(resultB) == paste0("Bcode.y", i)] <- paste0("Bcode", (i + 1))
  }
  
  # Result will contain the final aggregated correspondence table with hierarchical code columns for B
  resultB$test <- resultB[[paste0("Bcode", mostGranularB)]]
  resultB$Bsuperior <- NULL
  
  
  # Merge resultA and resultB using the 'test' column as the key
  Merged_AB <- merge(resultA, resultB, by.x = "test", by.y = "test", all = F)
  Merged_AB$test <- NULL
  
  
  ##Table merged 
  final_result <-Merged_AB
  ###4.2 Pairwise matching
  
  # Identify Acode and Bcode Columns
  acode_columns <- grep("^Acode", colnames(final_result), value = TRUE)
  bcode_columns <- grep("^Bcode", colnames(final_result), value = TRUE)
  
  # Loop Through Acode and Bcode Columns
  results_matrices <- list()
  
  for (acode_column in acode_columns) {
    level_Acode <- match(acode_column, acode_columns)
    
    for (bcode_column in bcode_columns) {
      level_Bcode <- match(bcode_column, bcode_columns)
      
      unique_combinations <- unique(final_result[, c(acode_column, bcode_column)])
      
      for (i in 1:nrow(unique_combinations)) {
        combination <- unique_combinations[i, ]
        
        # Perform the equality comparison using subset
        subset_data <- final_result[final_result[[acode_column]] == combination[[acode_column]] & 
                                      final_result[[bcode_column]] == combination[[bcode_column]], ]
        
        # Count Unique Occurrences
        count_Acode <- sapply(acode_columns, function(col) length(unique(subset_data[[col]])))
        count_Bcode <- sapply(bcode_columns, function(col) length(unique(subset_data[[col]])))
        
        # Build Results Matrix
        result_matrix <- c(level_Acode, level_Bcode, combination[[acode_column]], combination[[bcode_column]], count_Acode, count_Bcode)
        results_matrices <- append(results_matrices, list(result_matrix))
      }
    }
  }
  
  # Convert Matrices to Dataframe
  results_df <- as.data.frame(do.call(rbind, results_matrices))
  
  # Extract levels from Acode and Bcode columns
  acode_levels <- gsub("^Acode(\\d+)$", "\\1", acode_columns)
  bcode_levels <- gsub("^Bcode(\\d+)$", "\\1", bcode_columns)
  max_levels <- max(length(acode_levels), length(bcode_levels))
  acode_levels <- rep(acode_levels, length.out = max_levels)
  bcode_levels <- rep(bcode_levels, length.out = max_levels)
  # Define the new column names
  new_colnames <- c(paste0(ColumnNames_ab[1]," level"),
                    paste0(ColumnNames_ab[2]," level"),
                    ColumnNames_ab[1],
                    ColumnNames_ab[2],
                    paste0("N of ", ColumnNames_ab[1], acode_levels, " level values "),
                    paste0("N of ", ColumnNames_ab[2] , bcode_levels , " level values "))
  
  # Update column names in results_df
  colnames(results_df) <- new_colnames
  # Display Results
  
  if (!is.null(CSVout)) {
    # Using the testCsvParameter function to validate CSVout
    testCsvParameter("CSV", CSVout)
    
    # If CSVout is TRUE, generate the file name and proceed
    if (is.logical(CSVout) && CSVout == TRUE) {
      file_name <- paste0("AggregateCorrespondenceTable_", ColumnNames_ab[1], "_", ColumnNames_ab[2], ".csv")
      path_file <- file.path(getwd(), file_name)
      
      # Check for file existence and prompt for overwrite confirmation
      if (file.exists(path_file)) {
        cat("A CSV file with the same name already exists.\n")
        cat("Warning: This action will overwrite the existing file.\n")
        proceed <- readline("Do you want to proceed? (y/n): ")
        if (tolower(proceed) != "y") {
          cat("Operation aborted.\n")
          return(results_df)
        }
      }
      
      # Attempting to write to the CSV file with error handling
      tryCatch({
        write.csv(results_df, path_file, row.names = FALSE)
        cat("The table was saved in ", path_file, "\n")
      }, error = function(e) {
        cat("An error occurred while writing to the file:\n")
        cat(e$message, "\n")
        return(results_df)
      })
  
      }
  }

  
}
