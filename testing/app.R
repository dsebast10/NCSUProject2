# STATIC CODE -------------------------------------------------------------

library(shiny)
library(shinydashboard)
library(shinythemes)
library(tidyverse)
library(readxl)
library(plotly)

batch_import_files <- function(location) {
  # Imports and appends like excel files of data. Handles four different datasets
  #
  # Args:
  #   location: The path to the file that contains the data to be imported. The 
  #             data should be the only items in the file
  #
  # Returns two datasets:
  #   df_trace: a dataframe containing all of the trace data in the directory
  
  files <- dir(location)
  
  df_trace <- data.frame()
  
  for (i in files) {
    if (grepl("trace", i, ignore.case = T)){
      tmp_trace <- 
        read_excel(paste("Data/", i, sep = ""), skip = 1) %>%
        select(-`Source State`, -starts_with("total")) %>%
        mutate(Year = as.numeric(str_extract_all(i, "[:digit:]{4}"))) %>%
        rename(SourceState = X__1) %>% 
        filter(row_number() %in% 1:55) %>%
        gather(key = RecoveryState, value = Guns, ALABAMA:WYOMING) %>%
        filter(SourceState != "TOTAL", SourceState != "TOTALS")
      df_trace <- bind_rows(df_trace, tmp_trace)
    }
  }
  df_trace
  
}
traces <- batch_import_files("Data/")

# UI ----------------------------------------------------------------------

ui <- dashboardPage(skin = "red",
  dashboardHeader(),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction", tabName = "intro", icon = icon("comment")),
      radioButtons("source_recovery",
                   "Choose way of Looking at Data",
                   choices = c("Source States", "Recovery States")),
      menuItem("Map", tabName = "map", icon = icon("map")),
      menuItem("Time Chart", tabName = "chart", icon = icon("line-chart")),
      menuItem("Data Table", tabName = "table", icon = icon("table")),
      h3(" Filters"),
      selectizeInput("state",
                     "Select Recovery State",
                     selected = "DISTRICT OF COLUMBIA",
                     choices = levels(as.factor(traces$`RecoveryState`))),
      sliderInput("years", "Years", min = 2013, max = 2017, value = c(2013, 2017), step = 1, sep = ""),
      checkboxInput("exclude_state", "Exclude selected State in totals?")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "map",
              fluidPage(
                plotlyOutput("source_map")
              ))
    )
  )
)

# SERVER ------------------------------------------------------------------

server <- function(input, output, session){
  
  map_data <- reactive({
    guns <- traces %>%
      filter(RecoveryState == input$state) %>%
      {if (input$exclude_state) filter(., SourceState != input$state) else .}
      group_by(SourceState) %>%
      summarize(Guns = sum(Guns)) %>%
      mutate(SourceState = str_to_title(SourceState)) %>%
      filter(SourceState %in% state.name) %>%
      mutate(Abbr = setNames(state.abb, SourceState))
  })
  
  
  #update text on state selectize box
  observe({
    if(input$source_recovery == "Source States") {
      updateSelectizeInput(session, "state", label = "Select Source State")
    } else if(input$source_recovery == "Recovery States") {
      updateSelectizeInput(session, "state", label = "Select Recovery State")
    }
  })
  
  output$source_map <- renderPlotly({
    guns <- map_data()
    
    l <- list(color = toRGB("white"), width = 2)
    
    g <- list(
      scope = 'usa',
      projection = list(type = 'albers usa'),
      lakecolor = toRGB('white')
    )
    
    p <- plot_geo(guns, locationmode = 'USA-states') %>%
      add_trace(
        z = ~Guns,
        locations = ~Abbr,
        color = ~Guns, colors = 'YlGnBu'
      ) %>%
      colorbar(title = "Guns") %>%
      layout(geo = g)
    
    p
  })
}

# APP ---------------------------------------------------------------------

shinyApp(ui, server)