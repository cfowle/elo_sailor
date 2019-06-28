###############################################################################
### PROJECT:  ELO SAILOR
### CREATED:  2019-06-24
### MODIFIED: 2019-06-25
### REVIEWED: NO
### SUMMARY:  DEFINE FUNCTIONS THAT FOR THE RATING CREATION AND UPDATE PROCESS
###############################################################################

addCompetitors = function(existingRatings, newCompetitors) {
  ###HANDLES THE CREATIONS OF NEW COMPETITOR OBJECTS
  ###INPUTS:  exisitingRatings   list of ratings with competitor id as key
  ###         newCompetitors      data frame of newCompetitor details with columns
  ###                             competitorID, competitorName
  ###OUPUTS:                      updated ratings list
  
  ##check that newCompetitors object is as expected
  columnNames = c("competitorName", "competitorID")
  errorMessagePrefix = "ERROR IN addCompetitors: newCompetitors data frame must have column "
  for(name in columnNames){
    assert_that(name %in% colnames(newCompetitors), msg = paste0(errorMessagePrefix, name, "."))
  }
  
  ##create new competitor objects and add them to the ratings
  updatedRatings = existingRatings
  newCompetitors %<>% filter(!is.na(competitorID))
  if(nrow(newCompetitors) == 0){
    return(existingRatings)
  }
  
  for(i in 1:nrow(newCompetitors)){
    competitor = newCompetitors[i,]
    name = competitor$competitorName
    id = competitor$competitorID
    
    ##TODO: Think about whether or not this is the start condition we want
    rating = START_RATING
    initialRow = data.frame(0, -1, "Initial Value", 0, rating)
    names(initialRow) = c("day", "id", "name", "raceID", "rating")
    
    updatedRatings[id] = Competitor(id = id,
                                    name = name,
                                    currentRating = rating,
                                    ratings = initialRow)
  }
  
  return(updatedRatings)
}

checkForMissingCompetitors = function(existingRatings, results) {
  ###GETS LIST OF COMPETITORS IN RESULTS BUT NOT EXISTINGratingS
  ###INPUTS:  exisitingRatings   list of ratings with competitor id as key
  ###         results             data frame of results
  ###OUPUTS:                      data frame of competitor details with entities
  ###                             results and not existing ratings
  
  ##get list of competitors in each input
  ratingsCompetitors = names(existingRatings)
  resultsCompetitors  = unique(results$competitorID)
  
  ##find difference and return details table
  newCompetitors = setdiff(resultsCompetitors, ratingsCompetitors)
  competitorsTable = results %>%
    filter(competitorID %in% newCompetitors) %>%
    select(competitorID, competitorName) %>%
    distinct()
  
  return(competitorsTable)
}

processResult = function(existingRatings, result, regatta){
  ###UPDATES EXISITING ratingS GIVEN A RESULT
  ###INPUTS:  exisitingRatings   list of ratings with competitor id as key
  ###         result              data frame of a single result with columns
  ###                             raceID, competitorA, competitorB, win, scoreDiff
  ###         regatta             a Regatta Object  
  ###OUPUTS:                      updated ratings list
  
  ##check that inputs conform to constraints
  columnNames = c("raceID", "competitorA", "competitorB", "win", "scoreDiff")
  errorMessagePrefix = "ERROR IN processResult: result data frame must have column "
  for(name in columnNames){
    assert_that(name %in% colnames(result), msg = paste0(errorMessagePrefix, name, "."))
  }
  
  ##Get score prior to race
  competitorA = result$competitorA[[1]]
  competitorB = result$competitorB[[1]]
  ratingA = c(existingRatings[[competitorA]]$currentRating)
  ratingB = c(existingRatings[[competitorB]]$currentRating)
  
  ##Get K
  if(SCALE_K) {
    scoreDiff = result$scoreDiff[[1]]
    k = ifelse(scoreDiff == 0, 0, log(scoreDiff)*5) 
  } else {
    k = K
  }
  
  ##Run Elo update function
  win = c(result$win[[1]])
  eloOutput = elo.calc(win, ratingA, ratingB, k = k)
  print(competitorA)
  print(competitorB)
  print(eloOutput)
  
  ##Update ratings
  updatedRatings = existingRatings
  updatedRatings[[competitorA]] = updateRatings(existingRatings[competitorA],
                                                       eloOutput[1, 1],
                                                       regatta,
                                                       result$raceID[[1]])
  updatedRatings[[competitorB]] = updateRatings(existingRatings[competitorB],
                                                       eloOutput[1, 2],
                                                       regatta,
                                                       result$raceID[[1]])
  
  return(updatedRatings)
}

updateExistingRatings = function(existingRatings, regatta, results){
  ###UPDATES EXISITING RATINGS GIVEN A SET OF RESULTS
  ###INPUTS:  exisitingRatings   list of Ratings with competitor id as key
  ###         regatta             Regatta object for results
  ###         results             data frame of results from a single regatta
  ###                             with columns raceID, competitorID,
  ###                             competitorName, place, and score
  ###OUPUTS:                      updated ratings list
  
  ##check that inputs meet spec
  columnNames = c("raceID", "competitorID", "competitorName", "place", "score")
  errorMessagePrefix = "ERROR IN processResult: result data frame must have column "
  for(name in columnNames){
    assert_that(name %in% colnames(results), msg = paste0(errorMessagePrefix, name, "."))
  }
  
  ##check for competitors not in existing ratings & update accordingly
  missingCompetitors = checkForMissingCompetitors(existingRatings, results)
  updatedRatings     = addCompetitors(existingRatings, missingCompetitors)
  
  ##rewrite results pairwise
  pairwiseResults = createPairwiseComparisons(results)
  
  ##loop through results and update ratings
  for(i in 1:nrow(pairwiseResults)) {
    result = pairwiseResults[i, ]
    updatedRatings = processResult(updatedRatings, result, regatta)
  }
  
  return(updatedRatings)
}