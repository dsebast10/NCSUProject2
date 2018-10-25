library(shiny)
library(tidyverse)
library(magrittr)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  #filter data based on inputs
  updateData <- reactive({
    if(input$source_recovery == 'Where Guns Are Going' & input$include_state == F) {
      trace_data <- traces %>% filter(SourceState == input$state, 
                                       Year %in% input$years)
    } else if(input$source_recovery == 'Where Guns Are Going' & input$include_state == T){
      trace_data <- traces %>% filter(SourceState == input$state,
                                       RecoveryState != input$state,
                                       Year %in% input$years)
    } else if(input$source_recovery == 'Where Guns Are Coming From' & input$include_state == F){
      trace_data <- traces %>% filter(RecoveryState == input$state,
                                       Year %in% input$years)
    } else if(input$source_recovery == 'Where Guns Are Coming From' & input$include_state == T){
      trace_data <- traces %>% filter(RecoveryState == input$state,
                                       SourceState != input$state,
                                       Year %in% input$years)
    }
    return(trace_data)
  })
  
  tabularData <- reactive({
    if(input$source_recovery == 'Where Guns Are Going' & input$include_state == F) {
      trace_data <- traces %>% 
        filter(SourceState == input$state, Year %in% input$years) %>%
        group_by(RecoveryState) %>% 
        summarize(Guns = sum(Guns)) %>%
        mutate(`Percent of Total` = Guns/sum(Guns)*100) %>%
        arrange(desc(Guns))
    } else if(input$source_recovery == 'Where Guns Are Going' & input$include_state == T){
      trace_data <- traces %>% 
        filter(SourceState == input$state,RecoveryState != input$state, Year %in% input$years) %>%
        group_by(RecoveryState) %>% 
        summarize(Guns = sum(Guns)) %>%
        mutate(`Percent of Total` = Guns/sum(Guns)*100) %>%
        arrange(desc(Guns))
    } else if(input$source_recovery == 'Where Guns Are Coming From' & input$include_state == F){
      trace_data <- traces %>% 
        filter(RecoveryState == input$state, Year %in% input$years) %>%
        group_by(SourceState) %>% 
        summarize(Guns = sum(Guns)) %>%
        mutate(`Percent of Total` = Guns/sum(Guns)*100) %>%
        arrange(desc(Guns))
    } else if(input$source_recovery == 'Where Guns Are Coming From' & input$include_state == T){
      trace_data <- traces %>% 
        filter(RecoveryState == input$state, SourceState != input$state, Year %in% input$years) %>%
        group_by(SourceState) %>% 
        summarize(Guns = sum(Guns)) %>%
        mutate(`Percent of Total` = Guns/sum(Guns)*100) %>%
        arrange(desc(Guns))
    }
  })
  
  #update text on state selectize box
  observe({
    if(input$source_recovery == "Where Guns Are Going") {
      updateSelectizeInput(session, "state", label = "Select Source State")
    } else if(input$source_recovery == "Where Guns Are Coming From") {
      updateSelectizeInput(session, "state", label = "Select Recovery State")
    }
  })
   
  output$text <- renderText({
    paste("Checkbox is", input$include_state)
  })
  output$text2 <- renderText({
    unmappable_states <- c("GUAM", "TOTAL", "TOTALS", "US VIRGIN ISLANDS", "US VIRGIN  ISLANDS", "GUAM & NORTHERN MARIANA ISLANDS", "PUERTO RICO", "ALASKA", "HAWAII")
    trace_data <- tabularData() %>%
      filter(!(.[[1]] %in% unmappable_states))
    dim(trace_data)
  })
  
  output$table <- renderTable({
    tabularData()
   })
  output$map <- renderPlot({
    #get filtered data
    unmappable_states <- c("GUAM", "TOTAL", "TOTALS", "US VIRGIN ISLANDS", "US VIRGIN  ISLANDS", "GUAM & NORTHERN MARIANA ISLANDS", "PUERTO RICO", "ALASKA", "HAWAII")
    trace_data <- tabularData() %>%
      filter(!(.[[1]] %in% unmappable_states)) %>%
      mutate(genericState = tolower(.[[1]]))
    map <- map_data("state")
    state <- unique(tolower(traces[,1]))
    k <- ggplot(trace_data, aes(fill = Guns))
    k + geom_map(aes(map_id = state), map = map) + expand_limits(x = map$long, y = map$lat)
  })
  
})
