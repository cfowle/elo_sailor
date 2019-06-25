#############################################################################
### PROJECT:  ELO SAILOR
### CREATED:  2019-06-24
### MODIFIED: 2019-06-24
### REVIEWED: NO
### SUMMARY:  CREATES REGATTA CLASS AND RELATED FUNCTIONS
###############################################################################

Regatta = setRefClass("Regatta", 
                      fields = list(id              = "character",
                                    name            = "character",
                                    day             = "numeric",
                                    results         = "list"))

newRegatta = function(id, day, name, results, competitors) {
  scores = getCompetitorScores(results, competitors)
  regatta = Regatta(id = id,
          name = name,
          day = day,
          results = getResultList(results))
  return(regatta)
}