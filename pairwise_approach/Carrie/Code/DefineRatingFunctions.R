###############################################################################
### PROJECT:  ELO SAILOR
### CREATED:  2019-06-24
### MODIFIED: 2019-06-24
### REVIEWED: NO
### SUMMARY:  DEFINE FUNCTIONS THAT FOR THE RATING CREATION AND UPDATE PROCESS
###############################################################################

addCompetitors = function(existingRankings, newCompetitors) {
  ###HANDLES THE CREATIONS OF NEW COMPETITOR OBJECTS
  ###INPUTS:  exisitingRankings   list of rankings with competitor id as key
  ###         newCompetitors      data frame of newCompetitor details
  ###                             TODO: newCompetitors column spec
  ###OUPUTS:                      updated rankings list
  
  ##TODO: check that newCompetitors object is as expected
  ##TODO: create new competitor objects and add them to the rankings
  
  return(updatedRankings)
}

checkForMissingCompetitors = function(existingRankings, results) {
  ###GETS LIST OF COMPETITORS IN RESULTS BUT NOT EXISTINGRANKINGS
  ###INPUTS:  exisitingRankings   list of rankings with competitor id as key
  ###         results             data frame of results
  ###OUPUTS:                      data frame of competitor details with entities
  ###                             results and not existing rankings
  
  ##TODO: get list of competitors in each input
  ##TODO: find difference and return details table
  
  return(competitorsTable)
}

processResult = function(existingRankings, result){
  ###UPDATES EXISITING RANKINGS GIVEN A RESULT
  ###INPUTS:  exisitingRankings   list of rankings with competitor id as key
  ###         result              data frame of a single result
  ###                             TODO: results column spec
  ###OUPUTS:                      updated rankings list
  
  ##TODO: check that inputs conform to constraints
  ##TODO: run Elo update
  
  return(updatedRankings)
}

updateExistingRatings = function(existingRankings, results){
  ###UPDATES EXISITING RANKINGS GIVEN A SET OF RESULTS
  ###INPUTS:  exisitingRankings   list of rankings with competitor id as key
  ###         results             data frame of results
  ###                             TODO: results column spec
  ###OUPUTS:                      updated rankings list
  
  ##TODO: check that inputs conform to constraints
  ##TODO: check for competitors not in existing rankings
  ##TODO: update rankings with missing competitors
  ##TODO: loop through results and update rankings
  
  return(updatedRankings)
}