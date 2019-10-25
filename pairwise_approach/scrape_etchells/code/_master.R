###############################################################################
### MASTER FILE FOR SCRAPING ETCHELLS FLEET 20 SCORES
###############################################################################
library(assertthat)
library(dplyr)
library(magrittr)
library(readr)
library(rvest)
library(stringr)
library(tidyr)

setwd("~/code/elo_sailor/pairwise_approach/scrape_etchells/code")

source("get_links.R")
source("scrape_etchells_fleet20.R")
source("scrape_yacht_scoring.R")
source("loop_through_links.R")
source("clean_scores.R")
source("id_skippers.R")
source("scores_by_race.R")
source("define_pairwise_comparison.R")
source("get_pairwise_comparisons.R")