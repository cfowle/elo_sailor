###############################################################################
### PROJECT:  ELO SAILOR
### CREATED:  2019-06-24
### MODIFIED: 2019-06-24
### REVIEWED: NO
### SUMMARY:  RUNS ratingsS 
###############################################################################

##Import college test dataset
college = read_csv("../Input/neisa_only.csv")
college %<>% 
  mutate(X1 = NULL,
         raceID = 0,
         competitorName = school_coded) %>%
  rename(competitorID = school_coded) %>%
  group_by(regatta_id, day, raceID, competitorID, competitorName) %>%
  summarise(score = min(score), place = min(place)) %>%
  ungroup()

##TODO: Import clean results dataset
##TODO: If using existing ratingss, import existing ratingss table

##If starting fresh, create empty ratings list
results = college
existingRatings = list()

##Get list of regattas
regattaTable = results %>%
  select(regatta_id, day) %>%
  distinct() %>%
  mutate(name = regatta_id)

regattas = list()
for(i in 1:nrow(regattaTable)) {
  regatta = regattaTable[i,]
  id = as.character(regatta$regatta_id)
  day = regatta$day
  name = id
  
  if(!is.na(id)) {
    regattas[id] = newRegatta(id, day, name)
  }
}

##Run ratingss
for(regatta in regattas){
  id = regatta$id
  regattaResults = results %>%
    filter(regatta_id == id)
  existingRatings = updateExistingRatings(existingRatings, regatta, regattaResults)
}

##TODO: Export results
scaledratings = data.frame()
for(competitor in existingRatings) {
  print(competitor)
  scaledratings %<>% rbind(data.frame(competitor$id, competitor$currentRating))
}