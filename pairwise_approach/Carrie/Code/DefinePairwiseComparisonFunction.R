#############################################################################
### PROJECT:  ELO SAILOR
### CREATED:  2019-06-24
### MODIFIED: 2019-06-25
### REVIEWED: NO
### SUMMARY:  DEFINE PAIRWISE COMPARISON FUNCTION
###############################################################################

createPairwiseComparisons = function(results){
  ###INPUT:   results   datatable with raceID, competitorID, place,
  ###                   and score columns
  ###OUTPUT:            datatable of pairwise matchups for each raceID with the
  ###                   raceID, competitorA, competitorB, win, scoreDiff columns
  
  ##confirm input criteria are met
  columnNames = c("raceID", "competitorID", "place", "score")
  errorMessagePrefix = "ERROR IN createPairwiseComparisons: results data frame must have column "
  for(name in columnNames){
    assert_that(name %in% colnames(results), msg = paste0(errorMessagePrefix, name, "."))
  }
  
  ##create output data.frame
  matchups = data.frame(raceID = numeric,
                        competitorA = character,
                        competitorB = character,
                        win = numeric,
                        scoreDiff = numeric)
  
  ##loop through results to create pairwise comparison
  races = unique(results$raceID)
  for(race in races) {
    race = results %>%
      filter(raceID == race) %>%
      arrange(score)
    
    scores  = race$scores
    sailors = race$competitorID
    assert_that(length(sailors) == length(unique(sailors)),
                msg = "ERROR IN createPairwiseComparison: a single competitor id can be present only once in a given race.")
    
    for(i in 1:(length(sailors)-1)){
      for(j in (i + 1):length(sailors)){
        competitorA = sailors[[i]]
        competitorB = sailors[[j]]
        
        scoreA = scores[[i]]
        scoreB = scores[[j]]
        isWin = ifelse(scoreA > scoreB, 1, ifelse(scoreA == scoreB, 0.5), 0)
        scoreDiff = scoreA - scoreB
        
        matchups %<>% bind_rows(c(race, competitorA, competitorB, isWin, scoreDiff))
      }
    }
  }
  
  return(matchups)
}