###############################################################################
### PROJECT:  ELO SAILOR
### CREATED:  2019-08-20
### MODIFIED: 2019-08-20
### REVIEWED: NO
### SUMMARY:  RUNS RATINGS TO FIND IDEAL CORRECTION VALUE
###############################################################################

library(assertthat)
library(dplyr)
library(elo)
library(ggplot2)
library(magrittr)
library(plotly)
library(readr)

setwd("~/code/elo_sailor/pairwise_approach/Carrie/Code")

USE_EXISTING_RATINGS = FALSE
EXISTING_RATINGS_FILE = ""
RACE_LEVEL = TRUE
SCALE_K = FALSE
K = 42
START_RATING = 1500
IS_PAIRWISE = TRUE
CORRECTION_VALUE = 5

source("./DefinePairwiseComparisonFunction.R")
source("./DefineRatingFunctions.R")

##Define helper functions
fillMissing = function(pastRatings) {
  ##copies last available rating for all days in the dataset
  ids = unique(pastRatings$competitorID)
  maxDay = max(pastRatings$day)
  
  for(id in ids) {
    sailorRatings = pastRatings %>%
      filter(competitorID == id)
    
    lastT = 0
    lastRating = 1500
    if(nrow(sailorRatings) > 1) {
      for(i in 1:(nrow(sailorRatings) - 1)) {
        rating = sailorRatings[i, ]$rating
        day = sailorRatings[i, ]$day
        if(day - lastT < 2) {
          next()
        }
        filler = data.frame(rating = rep(lastRating, day - lastT - 1),
                            day = (lastT + 1):(day - 1),
                            competitorID = rep(id, day - lastT - 1))
        pastRatings %<>% bind_rows(filler)
        lastT = day
        lastRating = rating
      }
    }
    filler = data.frame(rating = rep(lastRating, maxDay - lastT),
                        day = (lastT + 1):maxDay,
                        competitorID = rep(id, maxDay - lastT))
    pastRatings %<>% bind_rows(filler)
  }
  return(pastRatings)
}

##Loop through values for CORRECTION_VALUE
raceResults = read.csv("../Input/etchells_races.csv") %>%
  select(place, competitorID, day) %>%
  mutate(competitorID = as.character(competitorID))

modelResults = data.frame()
for(i in 1:20) {
  print(i)
  CORRECTION_VALUE = i
  source("./RunRatings.R")
  
  testSet = fillMissing(pastRatings) %>%
    mutate(day = day - 1) %>%
    inner_join(raceResults) %>%
    mutate(competitorID = as.factor(competitorID))
  
  model = lm(place ~ rating, data = testSet)
  coeff = model$coefficients
  rsqrd = summary(model)$r.squared
  pvalue = summary(model)$coefficients[,4]
  
  row = data.frame(coeff[[2]], rsqrd, pvalue[[2]], CORRECTION_VALUE)
  modelResults %<>% bind_rows(row)
}

write.csv(modelResults, "../Output/fittingCorrection.csv")

maxRSqrd = max(results$rsqrd)
CORRECTION_VALUE = results %>%
  filter(rsqrd == maxRSqrd) %>%
  select(CORRECTION_VALUE)
CORRECTION_VALUE = CORRECTION_VALUE[[1]]

kResults = data.frame()
for(i in 5:50) {
  print(i)
  K = i
  source("./RunRatings.R")
  
  testSet = fillMissing(pastRatings) %>%
    mutate(day = day - 1) %>%
    inner_join(raceResults) %>%
    mutate(competitorID = as.factor(competitorID))
  
  model = lm(place ~ rating, data = testSet)
  coeff = model$coefficients
  rsqrd = summary(model)$r.squared
  pvalue = summary(model)$coefficients[,4]
  
  row = data.frame(coeff[[2]], rsqrd, pvalue[[2]], K)
  kResults %<>% bind_rows(row)
}

write.csv(kResults, "../Output/fittingK.csv")
