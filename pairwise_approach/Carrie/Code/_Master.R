#############################################################################
### PROJECT:  ELO SAILOR
### CREATED:  2019-06-24
### MODIFIED: 2019-06-24
### REVIEWED: NO
### SUMMARY:  MASTER FILE FOR PAIRWISE COMPARISON
###############################################################################

library(dplyr)
library(elo)
library(ggplot2)
library(magrittr)
library(plotly)
library(readr)

setwd("~/code/elo_sailor/pairwise_approach/Carrie/Code")

USE_EXISTING_RANKINGS = FALSE
EXISTING_RANKINGS_FILE = ""
RACE_LEVEL = TRUE
SCALE_K = TRUE
K = 20
START_RANKING = 200

source("./DefineCompetitorFunctions.R")
source("./DefineRegattaFunctions.R")
source("./DefinePairwiseComparisonFunction.R")
source("./DefineRatingFunctions.R")
source("./CleanResults.R")
source("./RunRatings.R")


source("./CleanResults.R")