###############################################################################
########         MASTER FILE FOR REGATTA-LEVEL RANKING METHOD          ######## 
###############################################################################
library(dplyr)
library(ggplot2)
library(magrittr)
library(plotly)
library(readr)

setwd("~/Desktop/Brainstorm/regatta_level_ranking_system")

DELTA_T = .25
N_INIT = 0.5

source("./DefineRegattaFunctions.R")
source("./DefineCompetitorFunctions.R")
source("./DefineRunnerFunctions.R")

