###############################################################################
### LOOPS THROUGH LINK LIST AND SCRAPES/COMPILES DATA
###############################################################################

links = read_csv("../intermediate/links.csv")

##clean up links table
##remove the fleet's scoring system pdfs
links %<>%
  filter(!str_detect(name, "eries")) %>%
  mutate(X1 = NULL) %>%
  mutate(year = str_extract(year, "^[0-9]*"))

##for each link scrape the scores
all_scores = data.frame()
for(i in 1:nrow(links)) {
  row = links[i,]
  link = row$link[[1]]
  print(link)
  
  if(str_detect(link, "yachtscoring")) {
    scores = scrape_yacht_scoring(row)
  } else {
    scores = scrape_etchells_fleet20(row) %>%
      mutate_all(as.character)
  }
  
  ##Pull everything together
  all_scores %<>% bind_rows(scores)
}

write.csv(all_scores, "../intermediate/scores.csv")
