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
for(i in 1:nrow(links)) {
  row = links[i,]
  link = row$link[[1]]
  scores = ifelse(str_detect("yachtscoring"),
                  scrape_yacht_scoring(row),
                  scrape_etchells_fleet20(row))
  
  ##TODO: Pull everything together
}