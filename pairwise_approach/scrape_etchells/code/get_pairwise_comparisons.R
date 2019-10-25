races = read_csv("../output/etchells_races.csv") %>%
  filter(!is.na(competitorID))

pairwise_races = data.frame()

for(regatta in unique(races$regatta_id)) {
  print(regatta)
  regatta_pairwise = races %>%
    filter(regatta_id == regatta) %>%
    createPairwiseComparisons() %>%
    mutate(regatta_id = regatta)
  
  pairwise_races %<>% rbind(regatta_pairwise)
}

write.csv(pairwise_races, "../output/etchells_races_pairwise.csv")
