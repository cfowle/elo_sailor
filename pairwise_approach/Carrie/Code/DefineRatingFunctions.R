###############################################################################
### PROJECT:  ELO SAILOR
### CREATED:  2019-06-24
### MODIFIED: 2019-06-25
### REVIEWED: NO
### SUMMARY:  DEFINE FUNCTIONS THAT FOR THE RATING CREATION AND UPDATE PROCESS
###############################################################################

checkInputColumns = function(df, functionName, columnNames) {
  ###CHECKS THAT DF HAS REQUIRED COLUMN NAMES 
  ###INPUT: df            test dataframe
  ###       functionName  string for error message
  ###       columnNames   vector of required column names
  
  errorMessagePrefix = "ERROR IN "
  errorMessageBody   = " : data frame must have column "
  for(name in columnNames){
    assert_that(name %in% colnames(df),
                msg = paste0(errorMessagePrefix, functionName, errorMessageBody, name, "."))
  }
}

addCompetitors = function(competitors, existingRatings, newCompetitors) {
  ###HANDLES THE CREATIONS OF NEW COMPETITOR OBJECTS
  ###INPUTS:  competitors         competitors data frame with columns competitorID and name
  ###         existingRatings     data frame with columns competitorID, regattaID, day, & rating
  ###         newCompetitors      data frame of newCompetitor details with columns
  ###                             competitorID, competitorName
  ###OUPUTS:                      updated ratings list
  
  ##check that newCompetitors object is as expected
  columnNames = c("name", "competitorID")
  checkInputColumns(newCompetitors, "addCompetitors", columnNames)
  
  ##create new competitor objects and add them to the ratings
  updatedCompetitors = competitors
  newCompetitors %<>% filter(!is.na(competitorID))
  if(nrow(newCompetitors) == 0){
    return(existingRatings)
  }
  
  updatedCompetitors %<>% rbind(newCompetitors)
  
  updatedRatings = existingRatings
  for(i in 1:nrow(newCompetitors)){
    competitorID = newCompetitors[i,1][[1]]
    regattaID = "Initial Value"
    day = 0
    rating = START_RATING
    
    initialRow = data.frame(competitorID, regattaID, day, rating)
    updatedRatings = updatedRatings %>%
      rbind(initialRow)
  }
  
  output = list("competitors" = updatedCompetitors, "ratings" = updatedRatings)
  return(output)
}

checkForMissingCompetitors = function(existingRatings, results) {
  ###GETS LIST OF COMPETITORS IN RESULTS BUT NOT EXISTINGratingS
  ###INPUTS:  exisitingRatings    data frame with columns competitorID, regattaID, day, & rating
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
    rename(name = competitorName) %>%
    distinct()
  
  return(competitorsTable)
}

processResult = function(existingRatings, pastRatings, result, regatta){
  ###UPDATES EXISITING ratingS GIVEN A RESULT
  ###INPUTS:  exisitingRatings    data frame with columns competitorID, regattaID, day, & rating
  ###         pastRatings         a dataframe with columns competitorID, regattaID, day, & rating
  ###         result              data frame of a single result with columns
  ###                             raceID, competitorA, competitorB, win, scoreDiff
  ###         regatta             a Regatta Object  
  ###OUPUTS:                      updated ratings list
  
  ##check that inputs conform to constraints
  columnNames = c("raceID", "competitorA", "competitorB", "win", "scoreDiff")
  checkInputColumns(result, "processResult", columnNames)
  
  ##Get score prior to race
  competitorA = as.character(result$competitorA[[1]])
  competitorB = as.character(result$competitorB[[1]])
  ratingA = c(filter(existingRatings, competitorID == competitorA)$rating[[1]])
  ratingB = c(filter(existingRatings, competitorID == competitorB)$rating[[1]])
  
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
  
  ##Move old ratings to pastRatings table
  oldRows = existingRatings %>%
    filter(competitorID %in% c(competitorA, competitorB))
  
  pastRatings %<>% rbind(oldRows)
  
  ##Update ratings
  updatedRatings = existingRatings %>%
    filter(!(competitorID %in% c(competitorA, competitorB)))
  
  newRowA = data.frame(competitorA, regatta$regattaID[[1]], regatta$day[[1]], eloOutput[1, 1])
  newRowB = data.frame(competitorB, regatta$regattaID[[1]], regatta$day[[1]], eloOutput[1, 2])
  colnames(newRowA) = colnames(updatedRatings)
  colnames(newRowB) = colnames(updatedRatings)
  
  updatedRatings %<>% 
    rbind(newRowA) %>%
    rbind(newRowB)
  
  output = list(current = updatedRatings, past = pastRatings)
  return(output)
}

updateExistingRatings = function(existingRatings, competitors, pastRatings, regatta, results){
  ###UPDATES EXISITING RATINGS GIVEN A SET OF RESULTS
  ###INPUTS:  exisitingRatings    a dataframe with columns competitorID, regattaID, day, & rating
  ###         competitors         a dataframe with columns competitorID and name
  ###         pastRatings         a dataframe with columns competitorID, regattaID, day, & rating
  ###         regatta             a dataframe with one row and columns regattaID, name, & day
  ###         results             data frame of results from a single regatta
  ###                             with columns raceID, competitorID,
  ###                             competitorName, place, and score
  ###OUPUTS:                      updated ratings list
  
  ##check that inputs meet spec
  checkInputColumns(existingRatings, "updateExisitingRatings", c("competitorID", "regattaID", "day", "rating"))
  checkInputColumns(competitors, "updateExisitingRatings", c("competitorID", "name"))
  checkInputColumns(pastRatings, "updateExisitingRatings", c("competitorID", "regattaID", "day", "rating"))
  checkInputColumns(regatta, "updateExisitingRatings", c("regattaID", "name", "day"))
  assert_that(nrow(regatta) == 1, msg ="Error in updateExistingRatings: df regatta should only contain one row.")
  
  ##check for competitors not in existing ratings & update accordingly
  missingCompetitors = checkForMissingCompetitors(existingRatings, results)
  output = addCompetitors(competitors, existingRatings, missingCompetitors)
  updatedRatings = output[["ratings"]]
  competitors = output[["competitors"]]
  
  updatedRatings %<>% mutate(competitorID = as.character(competitorID),
                             regattaID = as.character(regattaID))
  ##rewrite results pairwise
  pairwiseResults = createPairwiseComparisons(results)
  
  ##loop through results and update ratings
  for(i in 1:nrow(pairwiseResults)) {
    result = pairwiseResults[i, ]
    output = processResult(updatedRatings, pastRatings, result, regatta)
    updatedRatings = output[["current"]]
    pastRatings = output[["past"]]
  }
  
  return(updatedRatings)
}
