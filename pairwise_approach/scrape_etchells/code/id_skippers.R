###############################################################################
### IDENTIFY INDIVIDUAL SKIPPERS
###############################################################################

scores = read_csv("../intermediate/clean_scores.csv")
skippers = read_csv("../intermediate/skippers_id.csv") %>%
  mutate(X1 = NULL)

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

write.csv(scores, "../intermediate/scores_id.csv")
