###############################################################################
########               RUN REGATTA-LEVEL RANKING METHOD                ######## 
###############################################################################

results = read_csv("../neisa_only.csv")
results %<>% 
  mutate(competitorName = school_coded) %>%
  rename(competitorID = school_coded,
         id = regatta_id)

competitors = list()
competitors = updateCompetitors(results, competitors)

regattas = unique(results$id)
for(regatta in regattas) {
  if(!is.na(regatta)) {
    regattaResults = results %>%
      filter(id == regatta)
    
    competitors = runRegatta(regattaResults, competitors)
  }
}

scoreHistory = data.frame(day = numeric(),
                          name = character(),
                          score = numeric())
scores = data.frame(name  = character(),
                    score = numeric(),
                    stringsAsFactors = FALSE)
for(competitor in competitors) {
  scores %<>% 
    bind_rows(list(name = competitor$id, score = competitor$n))
  
  n_old = competitor$n_old %>%
    group_by(day) %>%
    summarise(score = mean(n_old)) %>%
    mutate(name = competitor$id)
  
  scoreHistory %<>%
    bind_rows(n_old)
}

frequentSailors = results %>%
  group_by(competitorID) %>%
  summarise(count = n()) %>%
  filter(count > 10)

frequentSailors = frequentSailors$competitorID
frequentHistory = filter(scoreHistory, name %in% frequentSailors)

scorePlot = ggplot(frequentHistory, aes(x = day, y = score, color = name)) +
  geom_line()

ggplotly(scorePlot)

averagePlaces = results %>%
  group_by(competitorID) %>%
  summarise(avg_place = mean(place, na.rm = TRUE)) %>%
  arrange(avg_place) %>%
  filter(competitorID %in% frequentSailors)

scores %<>%
  filter(name %in% frequentSailors)


