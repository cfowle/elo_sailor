###############################################################################
########              DEFINE REGATTA PROCESSING FUNCTIONS              ######## 
###############################################################################

getCompetitorScores = function(results, competitors) {
  ###IN : results       data.frame of results, one obs per competitor
  ###OUT:               list with key = competitor_id, and value = score
  
  inRegatta = results$competitorID
  scores = list()
  for(x in inRegatta) {
    competitor = competitors[[x]]
    scores[x] = competitor$n
  }
  
  return(scores)
}

getScoreDiffMatrix = function(results, scores) {
  ###IN : results       data.frame of results, one obs per competitor
  ###     score         list of each competitor's score
  ###OUT:               n x n matrix, where n is the number of competitors
  ###                   each cell represents the difference between i and j's scores
  
  names = names(scores)
  diffMatrix = matrix(nrow=length(scores), ncol = length(scores))
  rownames(diffMatrix) = names
  colnames(diffMatrix) = names
  
  for(i in 1:length(scores)) {
    for(j in 1:length(scores)) {
      n_i = scores[[i]]
      n_j = scores[[j]]
      diffMatrix[i,j] = n_i - n_j
    }
  }
  
  return(diffMatrix)
}

getResultList = function(results) {
  ###IN : results       data.frame of results, one obs per competitor
  ###OUT:               list with key = competitor_id, and value = place
  
  output = list()
  for(i in 1:nrow(results)){
    output[results[i,]$competitorID] = results[i,]$place
  }
  
  return(output)
}

getExpectedResultList = function(scores) {
  ###     score         list of each competitor's score
  ###OUT:               list with key = competitor_id, and value = E[Place|scores]
  
  scores = sort(unlist(scores), decreasing = TRUE)
  names  = names(scores)
  
  expectedResults   = list()
  unassignedIndices = c()
  unassignedIDs     = c()
  
  for(i in 1:length(scores)) {
    id     = names[i]
    idNext = names[i + 1]
    
    unassignedIndices = c(i, unassignedIndices)
    unassignedIDs     = c(id, unassignedIDs)
    
    if( i == length(scores) | scores[id] != scores[idNext]) {
      expected = mean(unassignedIndices)
      for(id in unassignedIDs) {
        expectedResults[id] = expected
      }
      unassignedIDs     = c()
      unassignedIndices = c()
    }
  }
  
  return(expectedResults)
}

###Define Regatta class
Regatta = setRefClass("Regatta", 
                      fields = list(id              = "character",
                                    day             = "numeric",
                                    diffMatrix      = "matrix",
                                    results         = "list",
                                    expectedResults = "list"))

newRegatta = function(id, day, results, competitors) {
  scores = getCompetitorScores(results, competitors)
  Regatta(id = id,
          day = day,
          diffMatrix = getScoreDiffMatrix(results, scores),
          results = getResultList(results),
          expectedResults = getExpectedResultList(scores))
}



