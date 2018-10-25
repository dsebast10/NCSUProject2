library(shiny)
library(tidyverse)
library(magrittr)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  #filter data based on inputs
  updateData <- reactive({
    if(input$source_recovery == 'Where Guns Are Going' & input$include_state == F) {
      trace_data <- traces %>% filter(SourceState == input$state, 
                                       Year %in% seq(input$years[1], input$years[2]))
    } else if(input$source_recovery == 'Where Guns Are Going' & input$include_state == T){
      trace_data <- traces %>% filter(SourceState == input$state,
                                       RecoveryState != input$state,
                                      Year %in% seq(input$years[1], input$years[2]))
    } else if(input$source_recovery == 'Where Guns Are Coming From' & input$include_state == F){
      trace_data <- traces %>% filter(RecoveryState == input$state,
                                      Year %in% seq(input$years[1], input$years[2]))
    } else if(input$source_recovery == 'Where Guns Are Coming From' & input$include_state == T){
      trace_data <- traces %>% filter(RecoveryState == input$state,
                                       SourceState != input$state,
                                      Year %in% seq(input$years[1], input$years[2]))
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
    2017 %in% input$years
  })
  
  output$table <- renderTable({
  tabularData()
   })
  
  output$short_table <- renderTable({
  head(tabularData)
  })
  
  output$time_chart <- renderPlot({
    
  })
  
  output$map <- renderPlot({
    
    us <- map_data("state")
    
    gg <- ggplot()
    gg <- gg + geom_map(data=us, map=us,
                        aes(x=long, y=lat, map_id=region),
                        fill="#ffffff", color="#ffffff", size=0.15)
    
    #get filtered data
    trace_data <- updateData()
    if(input$source_recovery == 'Where Guns Are Going'){
      trace_data %<>% 
        mutate(RecoveryState = tolower(RecoveryState)) %>%
        filter(RecoveryState %in% levels(as.factor(us$region))) %>%
        group_by(RecoveryState) %>% 
        summarize(Guns = sum(Guns)) %>%
        mutate(`Percent of Total` = Guns/sum(Guns)*100) %>%
        arrange(desc(Guns))
      
      gg <- gg + geom_map(data=trace_data, map=us,
                          aes(fill=Guns, map_id=RecoveryState),
                          color="#ffffff", size=0.15)
    } else if (input$source_recovery == 'Where Guns Are Coming From'){
      trace_data %<>% 
        mutate(SourceState = tolower(SourceState)) %>%
        filter(SourceState %in% levels(as.factor(us$region))) %>%
        group_by(SourceState) %>% 
        summarize(Guns = sum(Guns)) %>%
        mutate(`Percent of Total` = Guns/sum(Guns)*100) %>%
        arrange(desc(Guns))
      
      gg <- gg + geom_map(data=trace_data, map=us,
                          aes(fill=Guns, map_id=SourceState),
                          color="#ffffff", size=0.15)
    }
    gg <- gg + scale_fill_continuous(low='thistle2', high='darkred', 
                                     guide='colorbar')
    gg <- gg + labs(x=NULL, y=NULL)
    gg <- gg + coord_map("albers", lat0 = 39, lat1 = 45) 
    gg <- gg + theme(panel.border = element_blank())
    gg <- gg + theme(panel.background = element_blank())
    gg <- gg + theme(axis.ticks = element_blank())
    gg <- gg + theme(axis.text = element_blank())
    gg
  })
  
})
