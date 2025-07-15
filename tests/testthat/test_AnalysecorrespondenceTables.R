test_that("test_1 - analyseCorrespondenceTable works with valid data and A, B provided", {
  AB <- system.file("extdata/test", "TestAnalyse_ExempleAnnexe.csv", package = "correspondenceTables")
  A <- system.file("extdata/test", "TestAnalyse_SourceClassification.csv", package = "correspondenceTables")
  B <- system.file("extdata/test", "TestAnalyse_TargetClassification.csv", package = "correspondenceTables")
  
  result <- analyseCorrespondenceTable(AB, A = A, B = B, longestAcodeOnly = FALSE, longestBcodeOnly = FALSE)
  
  expect_true(is.list(result))
  expect_true(all(c("Inventory", "Analysis") %in% names(result)))
  expect_s3_class(result$Inventory, "data.frame")
  expect_s3_class(result$Analysis, "data.frame")
  
  expect_gt(nrow(result$Inventory), 0)
  expect_gt(nrow(result$Analysis), 0)
})

test_that("test_2 - analyseCorrespondenceTable emits warnings for unmatched codes in A and B", {
  AB <- system.file("extdata/test", "TestAnalyse_ExempleAnnexe.csv", package = "correspondenceTables")
  A_mismatch <- system.file("extdata/test", "TestAnalyse_SourceClassification_missing.csv", package = "correspondenceTables")
  B_mismatch <- system.file("extdata/test", "TestAnalyse_TargetClassification_missing.csv", package = "correspondenceTables")
  
  warnings <- capture_warnings(
    analyseCorrespondenceTable(
      AB = AB,
      A = A_mismatch,
      B = B_mismatch,
      longestAcodeOnly = FALSE,
      longestBcodeOnly = FALSE
    )
  )
  
  expect_true(
    any(grepl("Unmatched source classification codes in A", warnings, fixed = TRUE)),
    info = "Expected a warning about unmatched source codes"
  )
  
  expect_true(
    any(grepl("Unmatched target classification codes in B", warnings, fixed = TRUE)),
    info = "Expected a warning about unmatched target codes"
  )
})

test_that("test_3 - analyseCorrespondenceTable handles missing or empty AB file", {
  empty_AB <- system.file("extdata/test", "TestAnalyse_empty.csv", package = "correspondenceTables")
  
  expect_error(analyseCorrespondenceTable("nonexistent.csv"))
  expect_error(analyseCorrespondenceTable(empty_AB))
})

test_that("test_4 - analyseCorrespondenceTable correctly filters longestAcodeOnly and longestBcodeOnly", {
  AB <- system.file("extdata/test", "TestAnalyse_ExempleAnnexe.csv", package = "correspondenceTables")
  A <- system.file("extdata/test", "TestAnalyse_SourceClassification.csv", package = "correspondenceTables")
  B <- system.file("extdata/test", "TestAnalyse_TargetClassification.csv", package = "correspondenceTables")
  
  res <- analyseCorrespondenceTable(AB, A, longestAcodeOnly = TRUE)
  expect_true(nrow(res$Inventory) > 0)
  
  res <- analyseCorrespondenceTable(AB, B = B, longestBcodeOnly = TRUE)
  expect_true(nrow(res$Inventory) > 0)
  
  res <- analyseCorrespondenceTable(AB, A, B, longestAcodeOnly = TRUE, longestBcodeOnly = TRUE)
  expect_true(nrow(res$Inventory) > 0)
})

test_that("test_5 - analyseCorrespondenceTable detects duplicates in AB file", {
  AB_dup <- system.file("extdata/test", "ExempleAnnexe_duplicated.csv", package = "correspondenceTables")
  A <- system.file("extdata/test", "SourceClassification.csv", package = "correspondenceTables")
  B <- system.file("extdata/test", "TargetClassification.csv", package = "correspondenceTables")
  
  expect_error(analyseCorrespondenceTable(AB_dup, A = A, B = B),
               regexp = "Duplicate[s]?",
               ignore.case = TRUE)
})

test_that("test_6 - analyseCorrespondenceTable handles invalid input", {
  expect_error(analyseCorrespondenceTable("fichier_inexistant.csv"))
  expect_error(analyseCorrespondenceTable(AB = NULL))
  expect_error(analyseCorrespondenceTable(
    AB = system.file("extdata/test", "ExempleAnnexe.csv", package = "correspondenceTables"),
    longestAcodeOnly = "oui"
  ))
})

test_that("test_7 - filtering works for longestAcodeOnly and longestBcodeOnly", {
  AB <- system.file("extdata/test", "TestAnalyse_ExempleAnnexe.csv", package = "correspondenceTables")
  A <- system.file("extdata/test", "TestAnalyse_SourceClassification.csv", package = "correspondenceTables")
  B <- system.file("extdata/test", "TestAnalyse_TargetClassification.csv", package = "correspondenceTables")
  
  resultA <- analyseCorrespondenceTable(AB, A = A, longestAcodeOnly = TRUE)
  max_len_A <- max(nchar(unlist(strsplit(resultA$Inventory$SourcePositions, ", "))))
  expect_true(all(nchar(unlist(strsplit(resultA$Inventory$SourcePositions, ", "))) == max_len_A))
  
  resultB <- analyseCorrespondenceTable(AB, B = B, longestBcodeOnly = TRUE)
  max_len_B <- max(nchar(unlist(strsplit(resultB$Inventory$TargetPositions, ", "))))
  expect_true(all(nchar(unlist(strsplit(resultB$Inventory$TargetPositions, ", "))) == max_len_B))
})

test_that("test_8 - output structure of Inventory and Analysis is correct", {
  AB <- system.file("extdata/test", "ExempleAnnexe.csv", package = "correspondenceTables")
  result <- analyseCorrespondenceTable(AB)
  
  expect_true(is.list(result))
  expect_s3_class(result$Inventory, "data.frame")
  expect_s3_class(result$Analysis, "data.frame")
  
  expected_cols_inventory <- c("Component", "CorrespondenceType", "SourcePositions", "TargetPositions", "nSourcePositions", "nTargetPositions")
  expect_true(
    all(expected_cols_inventory %in% colnames(result$Inventory)),
    info = paste("Found columns in Inventory:", paste(colnames(result$Inventory), collapse = ", "))
  )
  
  expected_cols_analysis <- c("ClassA", "ClassB", "nTargetClasses", "SourceToTargetMapping", "nSourceClasses", "TargetToSourceMapping")
  expect_true(
    all(expected_cols_analysis %in% colnames(result$Analysis)),
    info = paste("Found columns in Analysis:", paste(colnames(result$Analysis), collapse = ", "))
  )
  
  expect_type(result$Inventory$Component, "character")
  expect_type(result$Inventory$nSourcePositions, "double")
})

test_that("test_9 - unmatched codes are correctly detected and reported", {
  AB <- system.file("extdata/test", "TestAnalyse_ExempleAnnexe.csv", package = "correspondenceTables")
  A_missing <- system.file("extdata/test", "TestAnalyse_SourceClassification_missing.csv", package = "correspondenceTables")
  B_missing <- system.file("extdata/test", "TestAnalyse_TargetClassification_missing.csv", package = "correspondenceTables")
  
  warnings <- capture_warnings({
    result <- analyseCorrespondenceTable(AB, A = A_missing, B = B_missing)
  })
  
  expect_true(
    any(grepl("Unmatched source classification codes in A", warnings, fixed = TRUE)),
    info = "Expected a warning about unmatched source codes"
  )
  expect_true(
    any(grepl("Unmatched target classification codes in B", warnings, fixed = TRUE)),
    info = "Expected a warning about unmatched target codes"
  )
  
  A_data <- read.csv(A_missing, stringsAsFactors = FALSE)
  AB_data <- read.csv(AB, stringsAsFactors = FALSE)
  B_data <- read.csv(B_missing, stringsAsFactors = FALSE)
  
  expect_true(length(setdiff(A_data[[1]], AB_data[[1]])) > 0)
  expect_true(length(setdiff(B_data[[1]], AB_data[[2]])) > 0)
})
