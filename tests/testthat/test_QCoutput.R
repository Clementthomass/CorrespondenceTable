# Test 1 - classificationQC() works with real NACE example
test_that("[1] classificationQC() works with real NACE example", {
  classification_file <- system.file("extdata", "Nace2.csv", package = "correspondenceTables")
  lengths_file <- system.file("extdata", "lenghtsNace.csv", package = "correspondenceTables")
  
  result <- classificationQC(
    classification = classification_file,
    lengthsFile = lengths_file,
    fullHierarchy = TRUE,
    labelUniqueness = TRUE,
    labelHierarchy = TRUE,
    singleChildCode = NULL,
    sequencing = NULL,
    CSVout = NULL
  )
  
  expect_type(result, "list")
  expect_true("QC_output" %in% names(result))
  expect_s3_class(result$QC_output, "data.frame")
})

# Test 2 - classificationQC() throws an error if classification is not a csv file
test_that("[2] classificationQC() throws an error if classification is not a csv file", {
  classification <- "./inst/extdata/invalid_classification.txt"
  lengths_path <- system.file("extdata", "lenghtsNace.csv", package = "correspondenceTables")
  
  expect_error(
    classificationQC(
      classification = classification,
      lengthsFile = lengths_path,
      fullHierarchy = FALSE,
      labelUniqueness = FALSE,
      labelHierarchy = FALSE
    ),
    regexp = "The classification should be provided as a csv file"
  )
})

# Test 3 - classificationQC() throws an error if lengthsFile is NA
test_that("[3] classificationQC() throws an error if lengthsFile is NA", {
  classification <- system.file("extdata", "Nace2.csv", package = "correspondenceTables")
  
  expect_error(
    classificationQC(
      classification = classification,
      lengthsFile = NA,
      fullHierarchy = FALSE,
      labelUniqueness = FALSE,
      labelHierarchy = FALSE
    ),
    regexp = "The provided lengths file is invalid or missing"
  )
})

# Test 4 - classificationQC() throws an error if lengthsFile has wrong extension
test_that("[4] classificationQC() throws an error if lengthsFile has wrong extension", {
  classification <- system.file("extdata", "Nace2.csv", package = "correspondenceTables")
  lengths_file <- system.file("extdata", "invalid_lengths.txt", package = "correspondenceTables")
  
  expect_error(
    classificationQC(
      classification = classification,
      lengthsFile = lengths_file,
      fullHierarchy = FALSE,
      labelUniqueness = FALSE,
      labelHierarchy = FALSE
    ),
    regexp = "The provided file does not have a CSV extension"
  )
})




# Test 5 - classificationQC() throws an error if duplicate codes are present
test_that("[5] classificationQC() throws an error if duplicate codes are present", {
  classification <- system.file("extdata", "nace_duplicate_code.csv", package = "correspondenceTables")
  lengthsFile <- system.file("extdata", "lenghtsNace.csv", package = "correspondenceTables")

  expect_error(
    classificationQC(
      classification = classification,
      lengthsFile = lengthsFile,
      fullHierarchy = FALSE,
      labelUniqueness = FALSE,
      labelHierarchy = FALSE
    ),
    regexp = "Codes in classification file must be unique"
  )
})
# Test 6 - classificationQC() throws an error if length segments overlap
test_that("[6] classificationQC() throws an error if length segments overlap", {
  classification_file <- system.file("extdata", "Nace2.csv", package = "correspondenceTables")
  lengths_file <- system.file("extdata", "nace_lengths_overlap.csv", package = "correspondenceTables")

  expect_error(
    classificationQC(
      classification = classification_file,
      lengthsFile = lengths_file,
      fullHierarchy = FALSE,
      labelUniqueness = FALSE,
      labelHierarchy = FALSE
    ),
    regexp = "Sequences should not overlap"
  )
})

test_that("[7] classificationQC() warns on duplicated labels", {
  classification <- system.file("extdata", "nace_duplicate_label.csv", package = "correspondenceTables")
  lengthsFile <- system.file("extdata", "lenghtsNace.csv", package = "correspondenceTables")
  
  expect_warning(
    classificationQC(
      classification = classification,
      lengthsFile = lengthsFile,
      fullHierarchy = FALSE,
      labelUniqueness = TRUE,
      labelHierarchy = FALSE
    ),
    regexp = "same labels.*QC_duplicatesLabel"
  )
})

# Test 8 - classificationQC() detects label hierarchy violations
test_that("[8] classificationQC() detects label hierarchy violations", {
  classification <- system.file("extdata", "nace_label_hierarchy_test.csv", package = "correspondenceTables")
  lengthsFile <- system.file("extdata", "lenghtsNace.csv", package = "correspondenceTables")
  
  expect_warning(
    classificationQC(
      classification = classification,
      lengthsFile = lengthsFile,
      fullHierarchy = FALSE,
      labelUniqueness = FALSE,
      labelHierarchy = TRUE
    ),
    regexp = "same label as their parent.*QC_singleChildMismatch"
  )
})


# Test 9 - classificationQC() detects single child code rule violations
test_that("[9] classificationQC() detects single child code rule violations", {
  classification <- system.file("extdata", "test_singleChildCode_classification.csv", package = "correspondenceTables")
  lengthsFile <- system.file("extdata", "lenghtsNace.csv", package = "correspondenceTables")
  singleChildCode <- system.file("extdata", "test_singleChildCode_rules.csv", package = "correspondenceTables")
  
  result <- suppressWarnings(classificationQC(
    classification = classification,
    lengthsFile = lengthsFile,
    fullHierarchy = FALSE,
    labelUniqueness = FALSE,
    labelHierarchy = FALSE,
    singleChildCode = singleChildCode
  ))
  
  expect_true("QC_singleCodeError" %in% names(result))
  expect_true("QC_multipleCodeError" %in% names(result))
  expect_gt(nrow(result$QC_singleCodeError), 0)
  expect_gt(nrow(result$QC_multipleCodeError), 0)
})


# 
# # Test 10 - classificationQC() detects sequencing rule violations
# test_that("[10] classificationQC() detects sequencing rule violations", {
#   classification <- system.file("extdata", "test_sequencing_classification.csv", package = "correspondenceTables")
#   lengthsFile <- system.file("extdata", "lenghtsNace.csv", package = "correspondenceTables")
#   sequencing <- system.file("extdata", "test_sequencing_rules.csv", package = "correspondenceTables")
#   
#   result <- suppressWarnings(classificationQC(
#     classification = classification,
#     lengthsFile = lengthsFile,
#     fullHierarchy = FALSE,
#     labelUniqueness = FALSE,
#     labelHierarchy = FALSE,
#     sequencing = sequencing
#   ))
#   
#   expect_true("QC_gapBefore" %in% names(result))
#   expect_gt(nrow(result$QC_gapBefore), 0)
# })


