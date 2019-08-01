library(dplyr)
library(DT)
library(ggplot2)
library(lubridate)
library(plotly)
library(readr)
library(shiny)

setwd("~/code/elo_sailor/pairwise_approach/Shiny App/Code")
source("./theme.R")



getRatingsHistogram = function(id, ratings) {
  competitorScore = filter(ratings, competitorID == id)$percentile[[1]]
  ratings_bins = ratings %>%
    mutate(bin = ifelse(percentile < competitorScore, 0,
                        ifelse(percentile > competitorScore, 2, 1))) %>%
    mutate(bin = as.factor(bin))
  histogram = ggplot(data = ratings_bins, aes(x=percentile, fill = bin)) +
    geom_histogram( bins = N_BINS) +
    scale_fill_limesight(palette = "sailing",
                         name = "Performance",
                         labels = c("Worse", id, "Better")) +
    ggtitle("Relative Rating") +
    ylab("'") +
    theme_limesight()
  return(histogram)
}

getTimeline = function(id, pastRatings) {
  combined = pastRatings %>%
    rbind(pastRatings) %>%
    mutate(color = ifelse(competitorID == id, 1, 0),
           size  = ifelse(competitorID == id, 2, 1)) %>%
    mutate(color = as.factor(color)) %>%
    filter(competitorID == id)
  
  timeline = ggplot(data = combined, aes(x = day,
                                         y = percentile,
                                         group = competitorID,
                                         color = color,
                                         size = size)) +
    geom_line() +
    scale_size(range = c(0.5, 1.5), guide="none") +
    scale_color_limesight(palette = "sailing",
                          name = "Sailor",
                          labels = c("Other Sailor", id)) +
    ggtitle("Rating over Time") +
    theme_limesight()
  return(timeline)
}