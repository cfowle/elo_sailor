###############################################################################
### MASTER FILE FOR SCRAPING ETCHELLS FLEET 20 SCORES
###############################################################################
library(dplyr)
library(magrittr)
library(rvest)
library(stringr)

setwd("~/code/elo_sailor/pairwise_approach/scrape_etchells/code")

source("get_links.R")
source("scrape_etchells_fleet20.R")
source("scrape_yacht_scoring.R")
source("loop_through_links.R")