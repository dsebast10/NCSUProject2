library(shiny)
library(tidyverse)
library(shinydashboard)
library(readxl)

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
        mutate(Year = str_extract_all(i, "[:digit:]{4}")) %>%
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

ui<- dashboardPage(
  dashboardHeader(title = "ATF Crime Gun Trace Info"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Map", tabName = "map", icon = icon("map")),
      menuItem("Time Chart", tabName = "chart", icon = icon("chart")),
      menuItem("Data Table", tabName = "table", icon = icon("table")),
      checkboxGroupInput("years",
                         "Years",
                         c(2017:2013),
                         selected = c(2017:2013)),
      radioButtons("source_recovery",
                   "Choose Data to Display on Map",
                   choices = c("Where Guns Are Going", "Where Guns Are Coming From")),
      selectizeInput("state",
                     "Select Recovery State",
                     selected = "DISTRICT OF COLUMBIA",
                     choices = levels(as.factor(traces$RecoveryState))),
      checkboxInput("include_state", "Exclude selected State in totals?")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "map"),
      tabItem(tabName = "chart"),
      tabItem(tabName = "table",
              fluidPage(
                box(tableOutput("table"))
              ))
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
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
  
}

shinyApp(ui, server)
