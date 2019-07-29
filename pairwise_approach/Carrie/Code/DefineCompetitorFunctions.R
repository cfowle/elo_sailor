###############################################################################
### PROJECT:  ELO SAILOR
### CREATED:  2019-06-24
### MODIFIED: 2019-06-24
### REVIEWED: NO
### SUMMARY:  CREATE COMPETITOR OBJECT AND UPDATE FUNCTION
###############################################################################

updateRatings = function(competitor, newRating, regatta, raceID = 0){
  ###IN:  competitor      Competitor object
  ###     newRating      Updated rating
  ###     regatta         Regatta Object containing regatta details
  ###     raceID          Number indicating which race this occurred in, 
  ###                     0 if regatta level rating
  ###OUT:                 updated competitor oject
  
  newRow = data.frame(regatta$day, regatta$id, regatta$name, raceID, newRating)
  names(newRow) = c("day", "id", "name", "raceID", "rating")
  
  newCompetitor = new("Competitor", 
                      id = competitor[[1]]$id,
                      name = competitor[[1]]$name,
                      currentRating = newRating,
                      ratings = rbind(competitor[[1]]$ratings, newRow))
    
  return(newCompetitor)
}

Competitor = setRefClass("Competitor",
                         fields = list(id = "character",
                                       name = "character",
                                       currentRating = "numeric",
                                       ratings = "data.frame"))
