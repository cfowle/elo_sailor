###############################################################################
########         MASTER FILE FOR REGATTA-LEVEL RANKING METHOD          ######## 
###############################################################################
library(dplyr)
library(ggplot2)
library(magrittr)
library(plotly)
library(readr)
library(VGAM)

setwd("~/code/elo_sailor/summary_approach/code")

DELTA_T = .8
ACTUAL_WEIGHT = -2.5
N_INIT = 0.5

source("./DefineRegattaFunctions.R")
source("./DefineCompetitorFunctions.R")
source("./DefineRunnerFunctions.R")
source("./RunResults.R")

