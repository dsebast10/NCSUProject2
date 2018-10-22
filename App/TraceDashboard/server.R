library(shiny)
library(tidyverse)

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
    2017 %in% input$years
  })
  
  output$table <- renderTable({
    trace_data <- updateData()
    if(input$source_recovery == 'Where Guns Are Going'){
      trace_data %>% 
        group_by(RecoveryState) %>% 
        summarize(Guns = sum(Guns)) %>%
        mutate(`Percent of Total` = Guns/sum(Guns)*100) %>%
        arrange(desc(Guns))
    } else if (input$source_recovery == 'Where Guns Are Coming From'){
      trace_data %>% 
        group_by(SourceState) %>% 
        summarize(Guns = sum(Guns)) %>%
        mutate(`Percent of Total` = Guns/sum(Guns)*100) %>%
        arrange(desc(Guns))
    }
   })
  output$map <- renderPlot({
    #get filtered data

    g <- ggplot()
  })
  
})
