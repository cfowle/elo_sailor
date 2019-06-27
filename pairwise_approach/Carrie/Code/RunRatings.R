###############################################################################
### PROJECT:  ELO SAILOR
### CREATED:  2019-06-24
### MODIFIED: 2019-06-24
### REVIEWED: NO
### SUMMARY:  RUNS RANKINGS 
###############################################################################

##Import college test dataset
college = read_csv("../Input/neisa_only.csv")
college %<>% mutate(X1 = NULL,
                    raceID = 0)

##TODO: Import clean results dataset
##TODO: If using existing rankings, import existing rankings table

##If starting fresh, create empty ranking list
results = college
exisitingRankings = list()

##Get list of regattas
regattaTable = results %>%
  select(regatta_id, day) %>%
  distinct() %>%
  mutate(name = regattaID)

regattas = list()
for(i in 1:nrow(regattaTable)) {
  regatta = regattaTable[i,]
  id = regatta$regatta_id
  day = regatta$day
  name = id
  
  regattas[id] = newRegatta(id, day, name)
}

##Run rankings
for(regatta in regattas){
  id = regatta$id
  regattaResults = results %>%
    filter(regatta_id == id)
  exisitingRankings = updateExistingRankings(exisitingRankings, regatta, regattaResults)
}

##TODO: Export results