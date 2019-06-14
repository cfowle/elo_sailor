###############################################################################
########                DEFINE COMPETITOR-LEVEL FUNCTIONS              ######## 
###############################################################################

getWeight = function(id, diffMatrix, z){
  ###IN:  id            unique identifier of competitor
  ###     diffMatrix    matrix represented the score differences between competitors
  ###     z             output from getZ for competitor x regatta
  ###OUT:               numeric weight for regatta difficulty
  
  row = diffMatrix[id,]
  j = length(row)
  ###TODO REDO WITH P YOU GOT THAT PLACE BASED OF A SKEW DISTRIBUTION
  w = 0
  for(competitor in 1:j){
    diff = row[competitor]
    if(diff*z > 0) {
      expected = 1
    } else {
      expected = -2
    }
    
    w = w + 1 - expected*diff
  }
   
  w = w/j
  return(w)
}

getZ = function(id, expectedResults, results){
  ###IN:  id                unique identifier of competitor
  ###     expectedResults   list of expected results
  ###     results           list of actual results
  ###OUT:                   difference between actual and expected results
  denominator = max(unlist(results))
  actual = results[[id]]/denominator
  expected = expectedResults[[id]]/denominator
  
  return(expected - actual)
}

getRegattaShifter = function(id, n, regatta){
  ###IN:  id        unique identifier of competitor
  ###     n         score prior to regatta
  ###     regatta   regatta object where competitor competed
  ###OUT:           the amount by which the regatta changes n
  
  z = getZ(id, regatta$expectedResults, regatta$results)
  w = getWeight(id, regatta$diffMatrix, z)
  return(z*w)
}

updateCompetitorScore = function(competitor, regatta){
  ###IN:  competitor      Competitor object
  ###     regatta         regatta object where competitor competed
  ###OUT:                 updated competitor oject
  
  id = competitor$id
  n  = competitor$n
  shifter = getRegattaShifter(id, n, regatta)
  n_new = n + DELTA_T*shifter
  
  day = regatta$day
  competitor$n = n_new
  competitor$n_old = rbind(c(day, n_new), competitor$n_old)
    
  return(competitor)
}

Competitor = setRefClass("Competitor",
                         fields = list(id = "character",
                                       name = "character",
                                       n = "numeric",
                                       n_old = "data.frame"))
