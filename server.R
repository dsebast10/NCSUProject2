library(shiny)
library(tidyverse)
library(magrittr)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  gun_data <- reactive({
    if (input$source_recovery == `Source State`){
      df <- 
        traces %>%
        filter(Year %in% seq(input$years[1], input$years[2]),
               `Recovery State` == input$state) %>%
               {if(input$exclude_state) filter(`Source State` != input$state) else .}
    } else {
      df <-
        traces %>%
        filter(Year %in% seq(input$years[1], input$years[2]),
               `Source State` == input$state) %>%
               {if(input$exclude_state) filter(`Recovery State` != input$state) else .}
    }
  })
  
  #filter data based on inputs
  updateData <- reactive({
    
    trace_data <- 
      traces %>%
      filter(`Source State` == input$state,
             if(input$exclude_state) {paste(input$source_recovery) != input$state},
             Year %in% seq(input$years[1], input$years[2]))
    # 
    # 
    # if(input$source_recovery == 'Where Guns Are Going' & input$exclude_state == F) {
    #   trace_data <- traces %>% filter(`Source State` == input$state, 
    #                                    Year %in% seq(input$years[1], input$years[2]))
    # } else if(input$source_recovery == 'Where Guns Are Going' & input$exclude_state == T){
    #   trace_data <- traces %>% filter(`Source State` == input$state,
    #                                    `Recovery State` != input$state,
    #                                   Year %in% seq(input$years[1], input$years[2]))
    # } else if(input$source_recovery == 'Where Guns Are Coming From' & input$exclude_state == F){
    #   trace_data <- traces %>% filter(`Recovery State` == input$state,
    #                                   Year %in% seq(input$years[1], input$years[2]))
    # } else if(input$source_recovery == 'Where Guns Are Coming From' & input$exclude_state == T){
    #   trace_data <- traces %>% filter(`Recovery State` == input$state,
    #                                    `Source State` != input$state,
    #                                   Year %in% seq(input$years[1], input$years[2]))
    # }
    return(trace_data)
  })
  tabularData <- reactive({
    if(input$source_recovery == 'Where Guns Are Going' & input$exclude_state == F) {
      trace_data <- traces %>% 
        filter(`Source State` == input$state, Year %in% input$years) %>%
        group_by(`Recovery State`) %>% 
        summarize(Guns = sum(Guns)) %>%
        mutate(`Percent of Total` = Guns/sum(Guns)*100) %>%
        arrange(desc(Guns))
    } else if(input$source_recovery == 'Where Guns Are Going' & input$exclude_state == T){
      trace_data <- traces %>% 
        filter(`Source State` == input$state,`Recovery State` != input$state, Year %in% input$years) %>%
        group_by(`Recovery State`) %>% 
        summarize(Guns = sum(Guns)) %>%
        mutate(`Percent of Total` = Guns/sum(Guns)*100) %>%
        arrange(desc(Guns))
    } else if(input$source_recovery == 'Where Guns Are Coming From' & input$exclude_state == F){
      trace_data <- traces %>% 
        filter(`Recovery State` == input$state, Year %in% input$years) %>%
        group_by(`Source State`) %>% 
        summarize(Guns = sum(Guns)) %>%
        mutate(`Percent of Total` = Guns/sum(Guns)*100) %>%
        arrange(desc(Guns))
    } else if(input$source_recovery == 'Where Guns Are Coming From' & input$exclude_state == T){
      trace_data <- traces %>% 
        filter(`Recovery State` == input$state, `Source State` != input$state, Year %in% input$years) %>%
        group_by(`Source State`) %>% 
        summarize(Guns = sum(Guns)) %>%
        mutate(`Percent of Total` = Guns/sum(Guns)*100) %>%
        arrange(desc(Guns))
    }
  })
  
  
  
  #update text on state selectize box
  observe({
    if(input$source_recovery == "Source State") {
      updateSelectizeInput(session, "state", label = "Select Source State")
    } else if(input$source_recovery == "Recovery State") {
      updateSelectizeInput(session, "state", label = "Select Recovery State")
    }
  })
   
  output$text <- renderText({
    paste("Checkbox is", input$exclude_state)
  })
  output$text2 <- renderText({
    2017 %in% input$years
  })
  
  output$table <- renderTable({
  gun_data()
   })
  
  output$short_table <- renderTable({
  gun_data() %>%
    arrange(desc(Guns))
  })
  
  output$time_table <- renderTable({
    updateData() %>%
      spread(key = Year, value = Guns) %>%
      mutate(Total = `2013`+`2014`+`2015`+`2016`+`2017`)
  })
  
  output$time_chart <- renderPlot({
    trace_data <-
      updateData() %>%
      spread(key = Year, value = Guns) %>%
      mutate(Total = `2013`+`2014`+`2015`+`2016`+`2017`) %>%
      arrange(desc(Total)) %>%
      filter(row_number() %in% 1:6) %>%
      gather()
    
    gg <- ggplot(trace_data)
    gg <- gg + geom_line(aes())
  })
  
  output$map <- renderPlot({
    
    us <- map_data("state")
    
    gg <- ggplot()
    gg <- gg + geom_map(data=us, map=us,
                        aes(x=long, y=lat, map_id=region),
                        fill="#ffffff", color="#ffffff", size=0.15)
    
    #get filtered data
    trace_data <- updateData()
    if(input$source_recovery == 'Recovery State'){
      trace_data %<>% 
        mutate(`Recovery State` = tolower(`Recovery State`)) %>%
        filter(`Recovery State` %in% levels(as.factor(us$region))) %>%
        group_by(`Recovery State`) %>% 
        summarize(Guns = sum(Guns)) %>%
        mutate(`Percent of Total` = Guns/sum(Guns)*100) %>%
        arrange(desc(Guns))
      
      gg <- gg + geom_map(data=trace_data, map=us,
                          aes(fill=Guns, map_id=`Recovery State`),
                          color="#ffffff", size=0.15)
    } else if (input$source_recovery == 'Source State'){
      trace_data %<>% 
        mutate(`Source State` = tolower(`Source State`)) %>%
        filter(`Source State` %in% levels(as.factor(us$region))) %>%
        group_by(`Source State`) %>% 
        summarize(Guns = sum(Guns)) %>%
        mutate(`Percent of Total` = Guns/sum(Guns)*100) %>%
        arrange(desc(Guns))
      
      gg <- gg + geom_map(data=trace_data, map=us,
                          aes(fill=Guns, map_id=`Source State`),
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
