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

##If starting fresh, create existingRatings, pastRatings, competitors, and
##regattas tables
results = college
existingRatings = data.frame(competitorID = character(),
                             regattaID = character(),
                             day = numeric(),
                             rating = numeric())

pastRatings = existingRatings

competitors = data.frame(competitorID = character(),
                         name = character())

##Get list of regattas
regattaTable = results %>%
  select(regatta_id, day) %>%
  distinct() %>%
  mutate(name = regatta_id) %>%
  rename(regattaID = regatta_id) %>%
  filter(!is.na(regattaID))

##Run ratingss
for(i in 1:nrow(regattaTable)){
  regatta = regattaTable[i,]
  id = regatta$regattaID[[1]]
  regattaResults = results %>%
    filter(regatta_id == id)
  output = updateExistingRatings(existingRatings,
                                 competitors,
                                 pastRatings,
                                 regatta,
                                 regattaResults)
  existingRatings = output[["current"]]
  pastRatings = output[["past"]]
}

##TODO: Export results
ratings = data.frame()
for(i in 1:nrow(existingRatings)) {
  competitor = existingRatings[i,]
  print(competitor)
  ratings %<>% bind_rows(data.frame(competitor$competitorID, competitor$rating))
}
