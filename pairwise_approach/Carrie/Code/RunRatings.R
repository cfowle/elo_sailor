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

##Import clean results dataset
if(IS_PAIRWISE) {
  pairwiseResults = read_csv("../Input/etchells_races_pairwise.csv") %>%
    rename(regattaID = regatta_id)
}

etchells = read_csv("../Input/etchells_races.csv")
##TODO: If using existing ratingss, import existing ratingss table

##If starting fresh, create existingRatings, pastRatings, competitors, and
##regattas tables
results = etchells %>%
  filter(!is.na(competitorID) & !is.na(score)) %>%
  arrange(regatta_id)

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
  print(id)
  regattaResults = results %>%
    filter(regatta_id == id)
  if(IS_PAIRWISE) {
    regattaResultsPairwise = filter(pairwiseResults, regattaID == id & !is.na(win))
  }
  
  output = updateExistingRatings(existingRatings,
                                 competitors,
                                 pastRatings,
                                 regatta,
                                 regattaResults,
                                 regattaResultsPairwise)
  existingRatings = output[["current"]]
  pastRatings = output[["past"]]
  competitors = output[["competitors"]]
}

##TODO: Export results
write.csv(existingRatings, "../Output/ratings_etchells.csv")
write.csv(pastRatings, "../Output/past_ratings_etchells.csv")
