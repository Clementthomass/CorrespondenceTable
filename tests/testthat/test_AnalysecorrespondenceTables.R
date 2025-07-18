test_that("test_1 - analyseCorrespondenceTable works with valid data and A, B provided", {
  AB <- system.file("extdata/test", "TestAnalyse_ExempleAnnexe.csv", package = "correspondenceTables")
  A <- system.file("extdata/test", "TestAnalyse_SourceClassification.csv", package = "correspondenceTables")
  B <- system.file("extdata/test", "TestAnalyse_TargetClassification.csv", package = "correspondenceTables")
  
  result <- analyseCorrespondenceTable(AB, A = A, B = B, longestAcodeOnly = FALSE, longestBcodeOnly = FALSE)
  
  # Check that the output is a list with expected components
  expect_true(is.list(result))
  expect_true(all(c("Inventory", "Analysis") %in% names(result)))
  expect_s3_class(result$Inventory, "data.frame")
  expect_s3_class(result$Analysis, "data.frame")
  
  # Ensure both outputs are not empty
  expect_gt(nrow(result$Inventory), 0)
  expect_gt(nrow(result$Analysis), 0)
  
  # Check that the expected columns are present in Analysis
  expected_cols_analysis <- c("Acode", "Bcode", "nTargetClasses", "SourceToTargetMapping",
                              "nSourceClasses", "TargetToSourceMapping")
  expect_true(all(expected_cols_analysis %in% colnames(result$Analysis)))
  
  # Check for a known correspondence pair
  expect_true(any(result$Analysis$Acode == "A1" & result$Analysis$Bcode == "B1"))
})

test_that("test_2 - analyseCorrespondenceTable emits warnings for unmatched codes in A and B", {
  AB <- system.file("extdata/test", "TestAnalyse_ExempleAnnexe.csv", package = "correspondenceTables")
  A_mismatch <- system.file("extdata/test", "TestAnalyse_SourceClassification_missing.csv", package = "correspondenceTables")
  B_mismatch <- system.file("extdata/test", "TestAnalyse_TargetClassification_missing.csv", package = "correspondenceTables")
  
  # Capture warnings for missing source codes
  warnings_A <- capture_warnings(
    analyseCorrespondenceTable(
      AB = AB,
      A = A_mismatch,
      B = NULL,
      longestAcodeOnly = FALSE,
      longestBcodeOnly = FALSE
    )
  )
  expect_true(any(grepl("Source classification codes in AB not found in A", warnings_A, fixed = TRUE)))
  
  # Capture warnings for missing target codes
  warnings_B <- capture_warnings(
    analyseCorrespondenceTable(
      AB = AB,
      A = NULL,
      B = B_mismatch,
      longestAcodeOnly = FALSE,
      longestBcodeOnly = FALSE
    )
  )
  expect_true(any(grepl("Target classification codes in AB not found in B", warnings_B, fixed = TRUE)))
})

test_that("test_3 - analyseCorrespondenceTable handles missing or empty AB file with clear messages", {
  empty_AB <- system.file("extdata/test", "TestAnalyse_empty.csv", package = "correspondenceTables")
  
  # Missing file error
  expect_error(
    analyseCorrespondenceTable("nonexistent.csv"),
    class = "error"
  )
  
  # Empty file error
  expect_error(
    analyseCorrespondenceTable(empty_AB),
    regexp = "No valid records found in the input correspondence table AB",
    fixed = TRUE
  )
})


test_that("test_4 - analyseCorrespondenceTable correctly filters longestAcodeOnly and longestBcodeOnly", {
  AB <- system.file("extdata/test", "TestAnalyse_longest_AB.csv", package = "correspondenceTables")
  A <- system.file("extdata/test", "TestAnalyse_longest_A.csv", package = "correspondenceTables")
  B <- system.file("extdata/test", "TestAnalyse_longest_B.csv", package = "correspondenceTables")
  
  # Test longest Acode only
  suppressWarnings({
    res_A <- analyseCorrespondenceTable(AB, A = A, longestAcodeOnly = TRUE)
    max_len_A <- max(nchar(res_A$Inventory$Acode))
    expect_true(all(nchar(res_A$Inventory$Acode) == max_len_A))
  })
  
  # Test longest Bcode only
  suppressWarnings({
    res_B <- analyseCorrespondenceTable(AB, B = B, longestBcodeOnly = TRUE)
    max_len_B <- max(nchar(res_B$Inventory$Bcode))
    expect_true(all(nchar(res_B$Inventory$Bcode) == max_len_B))
  })
  
  # Test both A and B
  suppressWarnings({
    res_both <- analyseCorrespondenceTable(AB, A = A, B = B, longestAcodeOnly = TRUE, longestBcodeOnly = TRUE)
    max_len_both_A <- max(nchar(res_both$Inventory$Acode))
    max_len_both_B <- max(nchar(res_both$Inventory$Bcode))
    expect_true(all(nchar(res_both$Inventory$Acode) == max_len_both_A))
    expect_true(all(nchar(res_both$Inventory$Bcode) == max_len_both_B))
  })
})


test_that("test_5 - analyseCorrespondenceTable detects duplicates in AB, A, and B files", {
  # File with duplicates in AB
  AB_dup <- system.file("extdata/test", "ExempleAnnexe_duplicated.csv", package = "correspondenceTables")
  A <- system.file("extdata/test", "SourceClassification.csv", package = "correspondenceTables")
  B <- system.file("extdata/test", "TargetClassification.csv", package = "correspondenceTables")
  
  # File with duplicates in A
  A_dup <- system.file("extdata/test", "SourceClassification_duplicated.csv", package = "correspondenceTables")
  
  # File with duplicates in B
  B_dup <- system.file("extdata/test", "TargetClassification_duplicated.csv", package = "correspondenceTables")
  
  # Check for duplicates in AB
  expect_error(
    analyseCorrespondenceTable(AB_dup, A = A, B = B),
    regexp = "Duplicate[s]?",
    ignore.case = TRUE
  )
  
  # Check for duplicates in A
  expect_error(
    analyseCorrespondenceTable(AB_dup, A = A_dup, B = B),
    regexp = "Duplicate[s]?",
    ignore.case = TRUE
  )
  
  # Check for duplicates in B
  expect_error(
    analyseCorrespondenceTable(AB_dup, A = A, B = B_dup),
    regexp = "Duplicate[s]?",
    ignore.case = TRUE
  )
})


test_that("test_6 - analyseCorrespondenceTable handles invalid input types", {
  valid_AB <- system.file("extdata/test", "TestAnalyse_ExempleAnnexe.csv", package = "correspondenceTables")
  
  # Check that the test file exists
  expect_true(file.exists(valid_AB))
  
  # Error if longestAcodeOnly is not a logical value
  expect_error(
    analyseCorrespondenceTable(
      AB = valid_AB,
      longestAcodeOnly = "YES"
    ),
    "Argument 'longestAcodeOnly' must be TRUE or FALSE"
  )
  
  # Error if longestBcodeOnly is not a logical value
  expect_error(
    analyseCorrespondenceTable(
      AB = valid_AB,
      longestBcodeOnly = 1
    ),
    "Argument 'longestBcodeOnly' must be TRUE or FALSE"
  )
})



test_that("test_7 - analyseCorrespondenceTable correctly filters with longestAcodeOnly and longestBcodeOnly (Inventory only)", {
  AB <- system.file("extdata/test", "TestAnalyse_longest_AB.csv", package = "correspondenceTables")
  A <- system.file("extdata/test", "TestAnalyse_longest_A.csv", package = "correspondenceTables")
  B <- system.file("extdata/test", "TestAnalyse_longest_B.csv", package = "correspondenceTables")
  
  # Filter by longest Acode only
  suppressWarnings({
    resultA <- analyseCorrespondenceTable(AB, A = A, longestAcodeOnly = TRUE)
    longest_A <- max(nchar(resultA$Inventory$Acode))
    expect_true(all(nchar(resultA$Inventory$Acode) == longest_A))
  })
  
  # Filter by longest Bcode only
  suppressWarnings({
    resultB <- analyseCorrespondenceTable(AB, B = B, longestBcodeOnly = TRUE)
    longest_B <- max(nchar(resultB$Inventory$Bcode))
    expect_true(all(nchar(resultB$Inventory$Bcode) == longest_B))
  })
  
  # Filter by both longest Acode and Bcode
  suppressWarnings({
    resultBoth <- analyseCorrespondenceTable(
      AB, A = A, B = B,
      longestAcodeOnly = TRUE,
      longestBcodeOnly = TRUE
    )
    expect_true(all(nchar(resultBoth$Inventory$Acode) == longest_A))
    expect_true(all(nchar(resultBoth$Inventory$Bcode) == longest_B))
  })
})
test_that("test_8 - output structure of Inventory and Analysis is correct", {
  AB <- system.file("extdata/test", "ExempleAnnexe.csv", package = "correspondenceTables")
  result <- analyseCorrespondenceTable(AB)
  
  # Check result is a list with correct names
  expect_true(is.list(result))
  expect_named(result, c("Inventory", "Analysis"))
  
  # Check Inventory structure
  expected_cols_inventory <- c("Component", "CorrespondenceType", "SourcePositions", "TargetPositions",
                               "nSourcePositions", "nTargetPositions")
  expect_s3_class(result$Inventory, "data.frame")
  expect_identical(colnames(result$Inventory), expected_cols_inventory)
  
  # Check types in Inventory
  expect_type(result$Inventory$Component, "character")
  expect_type(result$Inventory$nSourcePositions, "double")
  expect_type(result$Inventory$nTargetPositions, "double")
  expect_type(result$Inventory$SourcePositions, "character")
  expect_type(result$Inventory$TargetPositions, "character")
  
  # Check Analysis structure
  expected_cols_analysis <- c("ClassA", "ClassB", "nTargetClasses",
                              "SourceToTargetMapping", "nSourceClasses", "TargetToSourceMapping")
  expect_s3_class(result$Analysis, "data.frame")
  expect_identical(colnames(result$Analysis), expected_cols_analysis)
  
  # Check types in Analysis
  expect_type(result$Analysis$ClassA, "character")
  expect_type(result$Analysis$ClassB, "character")
})

test_that("test_9 - unmatched codes are correctly detected and reported", {
  # This test checks that analyseCorrespondenceTable emits appropriate warnings
  # when some codes in A and B are not used in AB
  
  AB <- system.file("extdata/test", "TestAnalyse_ExempleAnnexe.csv", package = "correspondenceTables")
  A_missing <- system.file("extdata/test", "TestAnalyse_SourceClassification_missing.csv", package = "correspondenceTables")
  B_missing <- system.file("extdata/test", "TestAnalyse_TargetClassification_missing.csv", package = "correspondenceTables")
  
  warnings <- capture_warnings({
    analyseCorrespondenceTable(AB, A = A_missing, B = B_missing)
  })
  
  expect_true(
    any(grepl("Unmatched source classification codes in A", warnings, fixed = TRUE)),
    info = "Expected a warning about unmatched source classification codes in A"
  )
  
  expect_true(
    any(grepl("Unmatched target classification codes in B", warnings, fixed = TRUE)),
    info = "Expected a warning about unmatched target classification codes in B"
  )
})

