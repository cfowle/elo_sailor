library(dplyr)
library(DT)
library(ggplot2)
library(lubridate)
library(magrittr)
library(plotly)
library(RColorBrewer)
library(readr)
library(shiny)

source("./theme.R")

ui <- fluidPage(
  includeCSS("../Source/style.css"),
  tags$head(
    tags$link(href="https://fonts.googleapis.com/css?family=Raleway|Staatliches", rel="stylesheet"),
    tags$title("Sailing Ratings")
  ),
  titlePanel(h1("Sailing Stats")),
  br(),
  selectizeInput("dataset", 
                 "Active Dataset",
                 c("College", "Laser"),
                 selected = "College",
                 multiple = FALSE,
                 options = NULL),
  tabsetPanel(
    tabPanel("Sample User View",
             br(),
             selectizeInput("activeCompetitor", 
                            "Active Sailor",
                            choices = NULL,
                            selected = NULL,
                            multiple = FALSE,
                            options = NULL),
             fluidRow(span(class = "element",column(5,
                                                    fluidRow(
                                                      column(5, h3("YOU RANK IN THE TOP", id = "callout")),
                                                      column(2, h3(textOutput("count"), id = "count")),
                                                      column(5)),
                                                    br(),
                                                    plotOutput("histogram")
             )),
                span(class = "element", column(7,plotOutput("plot")))
            ),
            h3("Results Considered"),
            DTOutput("resultsTable")
          ),
      tabPanel("Rating Table",
               DTOutput("ratingsTable")),
      tabPanel("RatingChart",
               br(),
               plotlyOutput("ratingsChart"))
    )
  )


server <- function(input, output, session) {
  
  ####LOAD IN DATA
  data <- reactiveValues()
  
  observeEvent(input$dataset,
               {
                 ##LOAD CURRENT  RATINGS
                 ratings <- read_csv(paste0("../Input/", input$dataset, "/ratings.csv"))
                 range = max(ratings$rating) - min(ratings$rating)
                 scale = min(ratings$rating)
                 
                 data$ratings = ratings %>%
                   mutate(rating = rating - scale) %>%
                   mutate(percentile = rating/range*100)
                 
                 ##LOAD PAST RATINGS
                 pastRatings <- read_csv(paste0("../Input/", input$dataset, "/pastRatings.csv"))
                 
                 range = max(pastRatings$rating) - min(pastRatings$rating)
                 scale = min(pastRatings$rating)
                 
                 data$pastRatings = pastRatings %>%
                   mutate(rating = rating - scale) %>%
                   mutate(percentile = rating/range*100)
                 
                 ##LOAD COMPETITORS
                 data$competitors = read_csv(paste0("../Input/",
                                                    input$dataset,
                                                    "/competitors.csv"))
                 
                 ##LOAD REGATTAS
                 data$regattas =read_csv(paste0("../Input/",
                                                input$dataset,
                                                "/regattas.csv"))
                 
                 ##LOAD RESULTS
                 data$results =read_csv(paste0("../Input/",
                                                input$dataset,
                                                "/results.csv"))
                 
                 if(input$dataset == "Laser") {
                   data$results %<>%
                     mutate(place = score) %>%
                     rename(day = start_date)
                   print(data$results)
                 }
                 
                 ##SET CHOICES
                 choices = unique(data$competitors$competitorID)
                 updateSelectizeInput(session,
                                      inputId = 'activeCompetitor',
                                      choices = choices,
                                      selected = choices[[1]],
                                      server = TRUE)
                 ##SET NBINS
                 data$bins = ifelse(input$dataset == "College", 13, 100)
               })
  
  output$count = renderText({
    percentile = filter(data$ratings, competitorID == input$activeCompetitor)$percentile[[1]]
    percentile = 100 - round(percentile)
    percentile = paste0(percentile, "%")
    
    return(percentile)
  })
  
  output$histogram = renderPlot({
    id = input$activeCompetitor
    N_BINS = data$bins
    
    competitorScore = filter(data$ratings, competitorID == id)$percentile[[1]]
    ratings_bins = data$ratings %>%
      mutate(bin = ifelse(percentile < competitorScore, 0,
                          ifelse(percentile > competitorScore, 2, 1))) %>%
      mutate(bin = as.factor(bin))
    
    ggplot(data = ratings_bins, aes(x=percentile, fill = bin)) +
      geom_histogram( bins = N_BINS) +
      scale_fill_limesight(palette = "sailing",
                           name = "Performance",
                           labels = c("Worse", id, "Better")) +
      ggtitle("Relative Rating") +
      ylab("'") +
      theme_limesight()
  })
  
  output$plot = renderPlot({
    id = input$activeCompetitor
    
    combined = data$ratings %>%
      rbind(data$pastRatings) %>%
      mutate(color = ifelse(competitorID == id, 1, 0),
             size  = ifelse(competitorID == id, 2, 1)) %>%
      mutate(color = as.factor(color))%>%
      mutate(regattaID = as.numeric(regattaID))
    
    if(input$dataset != "College") {
      combined %<>%
        filter(competitorID == id) %>%
        filter(day > 13500)
      labels = c(id)
      base = ggplot(data = combined, aes(x = day,
                                         y = percentile,
                                         group = competitorID,
                                         color = color,
                                         size = size)) 
    } else {
      base = ggplot(data = combined, aes(x = regattaID,
                                  y = percentile,
                                  group = competitorID,
                                  color = color,
                                  size = size)) 
      labels = c("Other Sailor", id)
    }
    
      base +
      geom_line() +
      scale_size(range = c(0.5, 1.5), guide="none") +
      scale_color_limesight(palette = "sailing",
                            name = "Sailor",
                            labels = labels) +
      ggtitle("Rating over Time") +
      theme_limesight()
  })
  
  output$resultsTable = renderDT(data$results %>%
    filter(competitorID == input$activeCompetitor) %>%
    arrange(desc(regatta_id), desc(raceID)) %>%
    select(regatta_id, raceID, day, place, score) %>%
    rename(Regatta = regatta_id,
           Race = raceID,
           Date = day,
           Place = place,
           Score = score)
  )
    
  
  output$ratingsTable = renderDT({
    data$ratings %>%
    inner_join(data$competitors, by="competitorID") %>%
    arrange(desc(percentile)) %>%
    select(competitorID, name, rating, percentile) %>%
    rename(Competitor = competitorID,
           Name = name,
           Rating = rating,
           Percentile = percentile) %>%
    mutate(Rating = round(Rating),
           Percentile = paste0(round(Percentile, 1), "%"))})
  
  output$ratingsChart = renderPlotly({
    combined = data$pastRatings %>%
      rbind(data$ratings) %>%
      mutate(regattaID = as.numeric(regattaID))
    
    ggplotly(ggplot(data = combined, aes(x = regattaID,
                                         y = percentile,
                                         color = reorder(competitorID, -percentile))) +
      geom_line()+
      labs(color = "Competitors"))
  })
  
}

shinyApp(ui=ui, server=server)
