###############################################################################
### PROJECT:  ELO SAILOR
### CREATED:  2019-06-24
### MODIFIED: 2019-06-24
### REVIEWED: NO
### SUMMARY:  CREATE COMPETITOR OBJECT AND UPDATE FUNCTION
###############################################################################

updateCompetitorScore = function(competitor, newRanking, regatta, raceID = 0){
  ###IN:  competitor      Competitor object
  ###     newRanking      Updated ranking
  ###     regatta         Regatta Object containing regatta details
  ###     raceID          Number indicating which race this occurred in, 
  ###                     0 if regatta level ranking
  ###OUT:                 updated competitor oject
  
  competitor$currentRank = newRanking
  competitor$rankings = rbind(c(regatta$day, regatta$id, regatta$name, raceID, newRanking),
                              competitor$rankings)
    
  return(competitor)
}

Competitor = setRefClass("Competitor",
                         fields = list(id = "character",
                                       name = "character",
                                       currentRank = "numeric",
                                       rankings = "data.frame"))
