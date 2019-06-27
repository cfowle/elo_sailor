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
                                    day             = "numeric"))

newRegatta = function(id, day, name) {
  regatta = Regatta(id = id,
          name = name,
          day = day)
  return(regatta)
}