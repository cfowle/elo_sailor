#############################################################################
### PROJECT:  ELO SAILOR
### CREATED:  2019-06-24
### MODIFIED: 2019-06-24
### REVIEWED: NO
### SUMMARY:  DEFINE PAIRWISE COMPARISON FUNCTION
###############################################################################

createPairwiseComparisons = function(results){
  ###INPUT:   results   datatable with raceID, competitorID, place,
  ###                   and score columns
  ###OUTPUT:            datatable of pairwise matchups for each raceID with the
  ###                   raceID, competitorA, competitorB, win, scoreDiff columns
  
  ##confirm inout criteria are met
  columnNames = c("raceID", "competitorID", "place", "score")
  errorMessagePrefix = "ERROR IN createPairwiseComparisons: results data frame must have column "
  for(name in columnNames){
    assert_that(name %in% colnames(results), msg = paste0(errorMessagePrefix, name, "."))
  }
  
  ##loop through results to create pairwise comparison
  
}