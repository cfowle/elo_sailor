###############################################################################
### PROJECT:  ELO SAILOR
### CREATED:  2019-06-24
### MODIFIED: 2019-06-25
### REVIEWED: NO
### SUMMARY:  DEFINE FUNCTIONS THAT FOR THE RATING CREATION AND UPDATE PROCESS
###############################################################################

addCompetitors = function(existingRankings, newCompetitors) {
  ###HANDLES THE CREATIONS OF NEW COMPETITOR OBJECTS
  ###INPUTS:  exisitingRankings   list of rankings with competitor id as key
  ###         newCompetitors      data frame of newCompetitor details with columns
  ###                             competitorID, competitorName
  ###OUPUTS:                      updated rankings list
  
  ##check that newCompetitors object is as expected
  columnNames = c("competitorName", "competitorID")
  errorMessagePrefix = "ERROR IN addCompetitors: newCompetitors data frame must have column "
  for(name in columnNames){
    assert_that(name %in% colnames(newCompetitors), msg = paste0(errorMessagePrefix, name, "."))
  }
  
  ##create new competitor objects and add them to the rankings
  updatedRankings = exisitingRankings
  for(i in 1:nrows(newCompetitors)){
    competitor = newCompetitors[i, ]
    name = competitor$competitorName
    id = competitor$competitorID
    ##TODO: Think about whether or not this is the start condition we want
    rank = START_RANKING 
    rankings = data.frame(day  = numeric,
                          id   = numeric,
                          name = character,
                          raceID = numeric,
                          newRanking = numeric)
    initialRow = c(0, -1, "Initial Value", 0, rank)
    
    updateRankings[id] = Competitor(id = id,
                                    name = name,
                                    currentRank = rank,
                                    rankings = rankings)
  }
  
  return(updatedRankings)
}

checkForMissingCompetitors = function(existingRankings, results) {
  ###GETS LIST OF COMPETITORS IN RESULTS BUT NOT EXISTINGRANKINGS
  ###INPUTS:  exisitingRankings   list of rankings with competitor id as key
  ###         results             data frame of results
  ###OUPUTS:                      data frame of competitor details with entities
  ###                             results and not existing rankings
  
  ##get list of competitors in each input
  rankingsCompetitors = names(exisitingRankings)
  resultsCompetitors  = unique(results$competitorID)
  
  ##find difference and return details table
  newCompetitors = setDiff(resultsCompetitors, rankingsCompetitors)
  competitorsTable = results %>%
    filter(competitorID %in% newCompetitors) %>%
    select(competitorID, competitorName) %>%
    distinct()
  
  return(competitorsTable)
}

processResult = function(existingRankings, result, regatta){
  ###UPDATES EXISITING RANKINGS GIVEN A RESULT
  ###INPUTS:  exisitingRankings   list of rankings with competitor id as key
  ###         result              data frame of a single result with columns
  ###                             raceID, competitorA, competitorB, win, scoreDiff
  ###         regatta             a Regatta Object  
  ###OUPUTS:                      updated rankings list
  
  ##check that inputs conform to constraints
  columnNames = c("raceID", "competitorA", "competitorB", "win", "scoreDiff")
  errorMessagePrefix = "ERROR IN processResult: result data frame must have column "
  for(name in columnNames){
    assert_that(name %in% colnames(results), msg = paste0(errorMessagePrefix, name, "."))
  }
  
  ##Get score prior to race
  competitorA = result$competitorA[[1]]
  competitorB = result$competitorB[[1]]
  scoreA = existingRankings[[competitorA]]$currentRank
  scoreB = existingRankings[[competitorB]]$currentRank
  
  ##Get K
  if(SCALE_K) {
    scoreDiff = result$scoreDiff[[1]]
    k = log(scoreDiff)
  } else {
    k = K
  }
  
  ##Run Elo update function
  win = result$win[[1]]
  updatedScores = elo.calc(win, scoreA, scoreB, k = k)
  
  ##Update rankings
  updatedRankings = existingRankings
  updatedRankings[competitorA] = updateCompetitorScore(existingRankings[competitorA],
                                                       updatedScores[1, 1],
                                                       regatta,
                                                       result$raceID[[1]])
  updatedRankings[competitorB] = updateCompetitorScore(existingRankings[competitorB],
                                                       updatedScores[1, 2],
                                                       regatta,
                                                       result$raceID[[1]])
  
  return(updatedRankings)
}

updateExistingRatings = function(existingRankings, regatta, results){
  ###UPDATES EXISITING RANKINGS GIVEN A SET OF RESULTS
  ###INPUTS:  exisitingRankings   list of rankings with competitor id as key
  ###         regatta             Regatta object for results
  ###         results             data frame of results from a single regatta
  ###                             with columns raceID, competitorID,
  ###                             competitorName, place, and score
  ###OUPUTS:                      updated rankings list
  
  ##check that inputs meet spec
  columnNames = c("raceID", "competitorID", "competitorName", "place", "score")
  errorMessagePrefix = "ERROR IN processResult: result data frame must have column "
  for(name in columnNames){
    assert_that(name %in% colnames(results), msg = paste0(errorMessagePrefix, name, "."))
  }
  
  ##check for competitors not in existing rankings & update accordingly
  missingCompetitors = checkForMissingCompetitors(existingRankings, results)
  updatedRakings     = addCompetitors(existingRankings, missingCompetitors)
  
  ##rewrite results pairwise
  pairwiseResults = createPairwiseComparisons(results)
  
  ##loop through results and update rankings
  for(i in 1:nrow(pairwiseResults)) {
    result = pairwiseResults[i, ]
    updatedRakings = processResult(updatedRankings, result, regatta)
  }
  
  return(updatedRankings)
}