% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/classificationQC.R
\name{classificationQC}
\alias{classificationQC}
\title{ClassificationQC performs quality control checks on statistical classifications}
\usage{
classificationQC(
  classification,
  lengthsFile,
  fullHierarchy = TRUE,
  labelUniqueness = TRUE,
  labelHierarchy = TRUE,
  singleChildCode = NULL,
  sequencing = NULL,
  CSVout = NULL
)
}
\arguments{
\item{classification}{Refers to a classification in a CSV file structured with two columns: "codes" and "labels." If the classification is provided as a CSV file. This argument is mandatory.}

\item{fullHierarchy}{It is used to test the fullness of hierarchy. If the parameter \code{fullHierarchy} is set to \code{FALSE}, the function will check that every position at a lower level than 1 should have parents all the way up to level 1. If set to \code{TRUE}, it will additionally check that any position at a higher level than k should have children all the way down to level k.}

\item{labelUniqueness}{It is used to test that positions at the same hierarchical level have unique labels. If set to \code{TRUE}, the compliance is checked, and positions with duplicate labels are marked as 1 in the "duplicateLabel" column, while positions with unique labels are marked as 0.}

\item{labelHierarchy}{It is used to ensure that the hierarchical structure of labels is respected. When set to \code{TRUE}, the function will check that single children have a label identical to the label of their parent and that if a position has a label identical to the label of one of its children, then that position should only have a single child.}

\item{singleChildCode}{It refers to a CSV file with specific formatting to define valid codes for each level. If this parameter is not \code{NULL}, then it checks compliance with coding rules for single children and non-single children, as provided in the CSV file.}

\item{sequencing}{It refers to a CSV file to define the admissible codes for multiple children at each level. If this parameter is not \code{NULL}, the function checks the sequencing of multiple children codes within each level, as provided in the CSV file.}

\item{CSVout}{The valid values are \code{NULL} means that the user don't need to write the parameters  \code{TRUE}. In both cases, the output will be returned as an R list. If the output should be saved as an CSV file, the argument should be set to \code{TRUE}. By default, no CSV file is produced.}

\item{lengthsfile}{Refers to a CSV file (one record per hierarchical level) containing the initial and last positions of the segment of the code specific to that level. The number of lines in this CSV file implicitly defines the number of hierarchical levels of the classification. This argument is mandatory.}
}
\value{
\code{classificationQC()} returns a list of data frames identifying possible cases violating the formatting requirements. The databases returned depend on the rules checked. The databases produced are:
- \code{QC_output}: The dataset includes all the original records in the classification. The "Level" column refers to the hierarchical levels of each position. Each code will be parsed into segment_k (column "Segmentk") and code_k (column "Codek"), corresponding to the code and segment at hierarchical level k, respectively. Additional columns are included to flag the behavior in each position:
  - \code{Orphan}: If \code{fullHierarchy} is set to \code{FALSE}, an "orphan" is a position at a hierarchical level (j) greater than 1 that lacks a parent at the hierarchical level (j-1) immediately above it. Orphan positions are marked with a value of 1 in the "Orphan" column, indicating their orphan status; otherwise, they are assigned a value of 0.
  - \code{Childless}: If \code{fullHierarchy} is set to \code{TRUE}, a "childless" position is one at a hierarchical level (j) less than k that lacks a child at the hierarchical level (j+1) immediately below it. Childless positions are marked with a value of 1 in the "Childless" column, indicating their childless status; otherwise, they are assigned a value of 0.
  - \code{DuplicateLabel}: A new column in the output that flags positions involved in duplicate label situations (where multiple positions share the same label at the same hierarchical level) by assigning them a value of 1, while positions with unique labels are assigned a value of 0.
  - \code{SingleChildMismatch}: A column in the output provides information about label hierarchy consistency in a hierarchical classification system. It indicates:
    - Value 1: Mismatched labels between a parent and its single child.
    - Value 9: Parent-child pairs with matching labels, but the parent has multiple children.
    - Value 0: Compliance with the label hierarchy rule, indicating no mismatches or violations.
  - \code{SingleCodeError}: A column serves as a flag indicating whether a position is a single child and whether the corresponding "singleCode" contains the level j segment. A value of 1 signifies a mismatch, while a value of 0 indicates compliance with the coding rules.
  - \code{MultipleCodeError}: A column serves as a flag indicating whether a position is not a single child and whether the corresponding "multipleCodej" contains the level j segment. A value of 1 signifies a mismatch, while a value of 0 indicates compliance with the coding rules.
  - \code{GapBefore}: The column is a flag that indicates whether there is a gap before a specific code within its level in the hierarchy. A gap refers to the absence of a sibling code that should logically precede the given code. In other words, it checks if there is a missing code in the sequence before a particular code.
  - \code{LastSibling}: The "LastSibling" column is a flag that identifies whether a code is the last sibling code at its level in the hierarchy. It is only relevant for codes with multiple children, meaning there are other codes at the same level with the same parent.

- \code{QC_noLevels}: A subset of the \code{QC_output} dataframe including only records for which levels are not defined. If this dataframe is not empty, it suggests that either the classification or the length file is not correctly specified.

- \code{QC_orphan}: A subset of the \code{QC_output} dataframe including only records that have no parents at the higher hierarchical level.

- \code{QC_childless}: A subset of the \code{QC_output} dataframe including only records that have no children at the lower hierarchical level.

- \code{QC_duplicatesLabel}: A subset of the \code{QC_output} dataframe including only records that have duplicated labels in the same hierarchical level.

- \code{QC_duplicatesCode}: A subset of the \code{QC_output} dataframe including only records that have the same codes.

- \code{QC_singleChildMismatch}: A subset of the \code{QC_output} dataframe including only records that are single children and have different labels from their parents or that are multiple children and have the same labels as their parents.

- \code{QC_singleCodeError}: A subset of the \code{QC_output} dataframe including only records that are single children and have been wrongly coded (not following the rule provided in the 'SingleChildMismatch' CSV file).

- \code{QC_multipleCodeError}: A subset of the \code{QC_output} dataframe including only records that are multiple children and have been wrongly coded (not following the rule provided in the 'SingleChildMismatch' CSV file).

- \code{QC_gapBefore}: A subset of the \code{QC_output} dataframe including only records that are multiple children and have a gap in the sequencing provided in the 'sequencing' CSV file.

- \code{QC_lastSibling}: A subset of the \code{QC_output} dataframe including only records that are multiple children and are the last sibling following the sequencing provided in the 'sequencing' CSV file.
}
\description{
The `classificationQC()` function performs quality control checks on statistical classifications, ensuring their integrity and accuracy. It checks compliance with various structural rules and provides informative error messages for violations. This function requires input files containing code and label information for each classification position and verifies the following criteria:
- Formatting requirements
- Uniqueness of codes
- Fullness of hierarchy
- Uniqueness of labels
- Hierarchical label dependencies
- Single child code compliance
- Sequencing of codes

The `classificationQC()` function generates a QC output data frame with the classification data, hierarchical levels, code segments, and test outcomes. Additionally, it allows exporting the output to a CSV file.
}
\examples{
{
# classification <- system.file("extdata", "Nace2.csv", package = "correspondenceTables")
  
  lengthsFile <- system.file("extdata", "lenghtsNace.csv", package = "correspondenceTables")
  
  Output <- classificationQC(classification = system.file("extdata", "Nace2.csv", package = "correspondenceTables") , lengthsFile = system.file("extdata", "lenghtsNace.csv", package = "correspondenceTables"), fullHierarchy = TRUE, labelUniqueness  = TRUE, labelHierarchy = TRUE, singleChildCode = NULL, sequencing = NULL, CSVout = system.file("extdata", "QC_Output.csv", package = "correspondenceTables")) 
  print(Output$QC_output)
  print(Output$QC_noLevels)
  print(Output$QC_orphan)
  print(Output$QC_childless)
  print(Output$QC_duplicatesLabel)
  print(Output$QC_duplicatesCode)
  print(Output$QC_singleChildMismatch)
  print(Output$QC_singleCodeError)
  print(Output$QC_multipleCodeError
    
 )
  print(Output$QC_gapBefore)
  print(Output$QC_lastSibling)
}
}
