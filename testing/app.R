# STATIC CODE -------------------------------------------------------------

library(shiny)
library(shinydashboard)
library(tidyverse)
library(readxl)
library(plotly)
library(magrittr)
library(DT)

batch_import_files <- function(location) {
  # Imports and appends like excel files of data. Handles four different datasets
  #
  # Args:
  #   location: The path to the file that contains the data to be imported. The 
  #             data should be the only items in the file
  #
  # Returns datasets:
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
  dashboardHeader(title = "Tracing Crime Guns"),
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
      checkboxInput("exclude_state", "Exclude selected State in totals?"),
      h4("Download Full Cleaned Dataset"),
      downloadButton('downloadData', 'Download')
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "intro",
              fluidPage(
                box(
                  htmlOutput("intro_html")
                )
              )),
      tabItem(tabName = "map",
              fluidPage(
                plotlyOutput("source_map", height = '800px'),
                textOutput("selection")
              )),
      tabItem(tabName = "chart",
              fluidPage(
                fluidRow(
                  plotlyOutput("source_time_chart")
                ),
                fluidRow(
                  plotlyOutput("destination_time_chart")
                )
              )),
      tabItem(tabName = "table",
              fluidPage(
                DT::dataTableOutput("cross_tab")
              ))
    )
  )
)

# SERVER ------------------------------------------------------------------

server <- function(input, output, session){

  #update text on state selectize box
  observe({
    if(input$source_recovery == "Source States") {
      updateSelectizeInput(session, "state", label = "Select Source State")
    } else if(input$source_recovery == "Recovery States") {
      updateSelectizeInput(session, "state", label = "Select Recovery State")
    }
  })
  
  #update state selection on plotly click
  observe({
    s <- event_data("plotly_click")
    if(is.null(s)) {
      return (NULL)
      } else {
    updateSelectizeInput(session, "state", selected = str_to_upper(state.name[as.numeric(s["pointNumber"]+1)]))
      }
  })
  
  #server info for download button
  output$downloadData <- downloadHandler(
    filename = function() {"full_trace_data_13_17.csv"},
    content = function(fname){
      write.csv(traces, fname, row.names = F)
    }
  )
  
  #setting up html output
  getPage<-function() {
    return(includeHTML("intro_page.html"))
  }
  output$intro_html<-renderUI({getPage()})
  
  #Source & Recovery Maps
  output$source_map <- renderPlotly({
    if(input$source_recovery == "Source States"){
      guns <- traces %>%
        filter(RecoveryState == input$state,
               Year %in% input$years[1]:input$years[2]) %>%
               {if (input$exclude_state) filter(., SourceState != input$state) else .} %>%
        group_by(SourceState) %>%
        summarize(Guns = sum(Guns)) %>%
        {if (input$exclude_state) rbind(.,c(input$state, 0)) else .} %>%
        mutate(SourceState = str_to_title(SourceState)) %>%
        filter(SourceState %in% state.name) %>%
        arrange(SourceState) %>%
        mutate(Abbr = setNames(state.abb, SourceState),
               Guns = as.numeric(Guns),
               lGuns = log(Guns+1))
      
      l <- list(color = toRGB("white"), width = 2)
  
      g <- list(
        scope = 'usa',
        projection = list(type = 'albers usa'),
        lakecolor = toRGB('white')
      )
      p <- plot_geo(guns, locationmode = 'USA-states') %>%
        add_trace(
          z = ~log(Guns+1),
          locations = ~Abbr,
          text = ~paste(Guns, "guns recovered in", str_to_title(input$state), "came from", SourceState),
          color = ~Guns, 
          colors = 'YlGnBu',
          showscale = FALSE,
          hoverinfo = 'text')%>%
        layout(geo = g,
               title = paste("Source States for Guns Recovered in", str_to_title(input$state)))
      p
    } else {
      guns <- traces %>%
        filter(SourceState == input$state,
               Year %in% input$years[1]:input$years[2]) %>%
               {if (input$exclude_state) filter(., RecoveryState != input$state) else .} %>%
        group_by(RecoveryState) %>%
        summarize(Guns = sum(Guns)) %>%
        {if (input$exclude_state) rbind(.,c(input$state, 0)) else .} %>%
        mutate(RecoveryState = str_to_title(RecoveryState)) %>%
        filter(RecoveryState %in% state.name) %>%
        arrange(RecoveryState) %>%
        mutate(Abbr = setNames(state.abb, RecoveryState),
               Guns = as.numeric(Guns),
               lGuns = log(Guns+1))
      
      l <- list(color = toRGB("white"), width = 2)
      
      g <- list(
        scope = 'usa',
        projection = list(type = 'albers usa'),
        lakecolor = toRGB('white')
      )
      br_pal <- colorRampPalette(c('blue','red'))
      p <- plot_geo(guns, locationmode = 'USA-states') %>%
        add_trace(
          z = ~log(Guns+1),
          locations = ~Abbr,
          text = ~paste(Guns, "guns recovered in", RecoveryState, "from origin state:", str_to_title(input$state)),
          color = ~Guns, 
          colors = 'YlGnBu',
          showscale = FALSE,
          hoverinfo = 'text')%>%
        layout(geo = g,
               title = paste("Where guns purchased in", str_to_title(input$state), "are being recovered."))
      p
    }
    
  })
  
  #UI updated text based on selection
  output$selection <- renderText({
    s <- event_data("plotly_click")
    if (length(s) == 0) {
      "Click on a state to select it."
    } else {
      paste("You selected:", str_to_title(state.name[as.numeric(s["pointNumber"]+1)]))
    }
  })
  
  #Chart of gun sources by time
  output$source_time_chart <- renderPlotly({
    top_5_states <- traces %>%
      filter(RecoveryState == input$state,
             Year %in% input$years[1]:input$years[2]) %>%
             {if (input$exclude_state) filter(., SourceState != input$state) else .} %>%
      group_by(SourceState) %>%
      summarize(Guns = sum(Guns)) %>%
      arrange(desc(Guns)) %>%
      select(-Guns) %>%
      head(5) %$%
      as.vector(t(.))
    
    guns <- traces %>%
      filter(RecoveryState == input$state,
             Year %in% input$years[1]:input$years[2]) %>%
             {if (input$exclude_state) filter(., SourceState != input$state) else .} %>%
      group_by(SourceState, Year) %>%
      summarize(Guns = sum(Guns)) %>%
      arrange(desc(Guns)) %>%
      filter(SourceState %in% top_5_states) 
    guns$SourceState <- with(guns, reorder(SourceState, -Guns))
    
    g <- ggplot(guns, aes(x=Year, y=Guns))+
      geom_line(aes(color=SourceState))+
      geom_point(aes(color=SourceState)) +
      guides(color = guide_legend(reverse = F)) +
      ggtitle(paste("Where guns recovered in", str_to_title(input$state), "were purchased, by Year Recovered"))
    g
  })
  
  #chart of gun destinations by year
  output$destination_time_chart <- renderPlotly({
    top_5_states <- traces %>%
      filter(SourceState == input$state,
             Year %in% input$years[1]:input$years[2]) %>%
             {if (input$exclude_state) filter(., RecoveryState != input$state) else .} %>%
      group_by(RecoveryState) %>%
      summarize(Guns = sum(Guns)) %>%
      arrange(desc(Guns)) %>%
      select(-Guns) %>%
      head(5) %$%
      as.vector(t(.))
    
    guns <- traces %>%
      filter(SourceState == input$state,
             Year %in% input$years[1]:input$years[2]) %>%
             {if (input$exclude_state) filter(., RecoveryState != input$state) else .} %>%
      group_by(RecoveryState, Year) %>%
      summarize(Guns = sum(Guns)) %>%
      arrange(desc(Guns)) %>%
      filter(RecoveryState %in% top_5_states) 
    guns$RecoveryState <- with(guns, reorder(RecoveryState, -Guns))
    
    g <- ggplot(guns, aes(x=Year, y=Guns))+
      geom_line(aes(color=RecoveryState))+
      geom_point(aes(color=RecoveryState)) +
      guides(color = guide_legend(reverse = F)) +
      ggtitle(paste("Where guns purchased in", str_to_title(input$state), "are being recovered, by Year Recovered"))
    g
  })
  
  #table showing raw data
  output$cross_tab <- DT::renderDataTable({
    DT::datatable(
      (traces %>%
        filter(Year %in% input$years[1]:input$years[2]) %>%
        group_by(SourceState, RecoveryState) %>%
        summarize(Guns = sum(Guns)) %>%
        spread(RecoveryState, Guns)),
      extensions = list("Scroller", "FixedColumns", "Buttons"),
      options = list(dom = 'Bfrtip', scrollX = T, scrollY = T, scroller = T, fixedColumns = T, buttons = c('csv', 'excel'))
      )
  })
}

# APP ---------------------------------------------------------------------

shinyApp(ui, server)