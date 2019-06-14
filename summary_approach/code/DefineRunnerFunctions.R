###############################################################################
########               DEFINE RATINGS RUNNER FUNCTIONS                 ######## 
###############################################################################

updateCompetitors = function(results, competitors) {
  allCompetitors = results %>%
    group_by(competitorID, competitorName) %>%
    summarize(count = n())
  
  newCompetitors = allCompetitors %>%
    filter(!competitorID %in% allCompetitors)
  
  for(i in 1:nrow(newCompetitors)) {
    new = Competitor(id = newCompetitors[i,]$competitorID,
                     name = newCompetitors[i,]$competitorName,
                     n = N_INIT,
                     n_old = data.frame(list("day" = 0, "n_old" = N_INIT)))
    competitors[newCompetitors[i,]$competitorID] = new
  }
  
  return(competitors)
}

runRegatta = function(results, competitors) {
  regattaInfo = results %>%
    group_by(id, day) %>%
    summarise(count = n())
  
  regatta = newRegatta(id = as.character(regattaInfo$id),
                       day = regattaInfo$day,
                       results = results,
                       competitors = competitors)
  
  attendees = results$competitorID
  for(attendee in attendees){
    updateCompetitorScore(competitors[[attendee]], regatta)
  }
  
  return(competitors)
}
