###############################################################################
### CLEANS RESULTS SCRAPED FROM ETCHELLS20 RESULTS PAGE
###############################################################################

scores = read_csv("../intermediate/scores.csv") %>%
  mutate(X1 = NULL)

condenseRaces = function(scores, raceNum) {
##For a given race, raceNum, find all columns with relevant data in scores
##and collapse into a single column raceNum
##Returns version of scores with collapsed column
  raceNum = as.character(raceNum)
  raceResults = scores %>%
    select(contains(raceNum)) %>%
    unite(race, sep = "") %>%
    mutate(race = str_remove_all(race, "NA")) %>%
    mutate(race = ifelse(race == "", NA, race)) %>%
    rename(!!(paste0("r", raceNum)) := race)
  
  scores %<>% 
    select(-contains(raceNum)) %>%
    bind_cols(raceResults)
  
  return(scores)
}

condenseList = function(scores, testString, colName) {
  ##For all columns matching regex expression, testString,
  ##collapse into a single column colName
  ##Returns version of scores with collapsed column
  listResults = scores[, grepl(testString, colnames(scores))]
  listResults %<>%
    unite(col, sep = "") %>%
    mutate(col = str_remove_all(col, "NA")) %>%
    mutate(col = ifelse(col == "", NA, col)) %>%
    rename(!!(colName) := col)
  
  scores = scores[, -grep(testString, colnames(scores))] %>%
    bind_cols(listResults)
  
  return(scores)
}

coerceToNumeric = function(scores, column) {
  ##For column, extract numeric values and convert to numeric
  ##Return scores with updated column
  
  updatedScores = scores %>%
    rename(column = !!(column)) %>%
    mutate(column = str_extract(column, "[0-9]*\\.?[0-9]*")) %>%
    mutate(column = ifelse(column == "", NA, as.numeric(column))) %>%
    rename(!!(column) := column)
  
  return(updatedScores)
}

##Collapse race columns
for(i in 1:8) {
  scores = condenseRaces(scores, i) %>%
    coerceToNumeric(paste0("r", i))
}

##Split up bow and sailnumber where needed
scores %<>% 
  mutate(bow = ifelse(is.na(`bow/sail`),
                      NA,
                      str_extract(`bow/sail`, "^[0-9]*")),
         sail = ifelse(is.na(`bow/sail`),
                       NA,
                       str_extract(`bow/sail`, "(?<=/).*"))) %>%
  mutate(`bow/sail` = NULL)

##Split up skipper and crew where needed
scores %<>%
  mutate(skipper = ifelse(is.na(`skipper/crew`),
                          skipper,
                          str_extract(`skipper/crew`, "^.*(?= /)")),
         crew = ifelse(is.na(`skipper/crew`),
                       crew,
                       str_extract(`skipper/crew`, "(?<=/ ).*$"))) %>%
  mutate(`skipper/crew` = NULL)

##Define test strings
totalTestString = "tot|ttl"
boatNameTestString = "boat|yacht name"
sailTestString = "sail|other"
bowTestString = "bow|#"
clubTestString = "club|from"
placeTestString = "to$|pos|pl|pt"
skipperTestString = "owner|skipper$"

##Collapse columns using test strings
scores %<>% 
  condenseList(totalTestString, "total") %>%
  condenseList(boatNameTestString, "boat") %>%
  condenseList(sailTestString, "sail") %>%
  condenseList(bowTestString, "bow") %>%
  condenseList(clubTestString, "club") %>%
  condenseList(placeTestString, "place") %>%
  condenseList(skipperTestString, "skipper")

##Remove meaningless rows
scores %<>%
  coerceToNumeric("total") %>%
  coerceToNumeric("place") %>%
  filter(!is.na(total) & !is.na(r1))

write.csv(scores, "../intermediate/clean_scores.csv")

