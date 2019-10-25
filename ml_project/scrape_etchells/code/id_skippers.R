###############################################################################
### IDENTIFY INDIVIDUAL SKIPPERS
###############################################################################

scores = read_csv("../intermediate/clean_scores.csv")
skippers = read_csv("../intermediate/skippers_id.csv") %>%
  mutate(X1 = NULL)

##clean up skipper and match with ID'd list
scores %<>%
  mutate(skipper_raw = skipper,
         skipper = tolower(skipper),
         boat    = tolower(boat)) %>%
  mutate(skipper = str_remove_all(skipper, "(?<= /) *")) %>%
  mutate(skipper = str_remove_all(skipper, " *(?=/)")) %>%
  mutate(skipper = ifelse(str_detect(skipper, "/"),
                          str_extract(skipper, "[\\w ]*(?=/)"),
                          skipper)) %>%
  mutate(skipper = ifelse(str_detect(skipper, ", "),
                          paste(str_extract(skipper, "^\\w*"), str_extract(skipper, "\\w*$")),
                          skipper)) %>%
  mutate(skipper = trimws(skipper)) %>%
  mutate(lastName = str_extract(skipper, "\\w*$")) %>%
  full_join(skippers)

##remove bad dupes
scores %<>%
  filter(!str_detect(boat, "corinthian")) %>%
  filter(url != "http://www.etchellsfleet20.org/results/2010/2010cryccup.html" | (name == "Coral Reef Cup" & year == 2010))

##get skipper name by id
##use heuristic of longest name for each id
names = scores %>%
  group_by(id) %>%
  summarise(competitorName = skipper[which(length(skipper) == max(length(skipper)))])

scores %<>% inner_join(names)

write.csv(scores, "../intermediate/scores_id.csv")