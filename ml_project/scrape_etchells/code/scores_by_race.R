###############################################################################
### FLIP BY REGATTA RESULTS TO BE BY RACE
###############################################################################

scores = read_csv("../intermediate/scores_id.csv") %>%
  mutate(X1 = NULL,
         X1_1 = NULL)

##rotate to be by race
scores %<>%
  gather(score = r1:r8) %>%
  rename(raceID = key,
         score = value) %>%
  mutate(place = NULL)

##get regatta ids
ids = scores %>%
  arrange(year) %>%
  select(url) %>%
  distinct()
ids$regatta_id = seq.int(nrow(ids))

scores %<>%
  inner_join(ids) %>%
  mutate(day = regatta_id,
         place = score) %>%
  rename(competitorID = id)

write.csv(scores, "../output/etchells_races.csv")
