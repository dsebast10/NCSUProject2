# Libraries -------------------------------------------------------------

library(shiny)
library(shinydashboard)
library(tidyverse)
library(readxl)
library(plotly)
library(magrittr)
library(DT)
library(pdftools)


# Static Data Handling ----------------------------------------------------

source('data_import.R')

# UI ----------------------------------------------------------------------



ui <- dashboardPage(skin = "red",
  dashboardHeader(title = "Tracing Crime Guns To and From States",
                  titleWidth = 450),
  dashboardSidebar(width = 350,
    sidebarMenu(
      menuItem("Introduction", tabName = "intro", icon = icon("comment"), selected = T),
      radioButtons("source_recovery",
                   "Choose Data to Display on Map",
                   choices = c("Source States", "Recovery States")),
      menuItem("Map", tabName = "map", icon = icon("map")),
      menuItem("Time Chart", tabName = "chart", icon = icon("calendar")),
      menuItem("Data Table", tabName = "table", icon = icon("table")),
      menuItem("Gun Laws", tabName = "cato", icon = icon("crosshairs")),
      menuItem("Linear Models", tabName = "linear", icon = icon("line-chart")),
      menuItem("Tree Model", tabName = "tree", icon = icon("tree")),
      menuItem("PCA", tabName = "pca", icon = icon("compass")),
      h3(" Filters"),
      selectizeInput("state",
                     "Select Recovery State",
                     selected = "DISTRICT OF COLUMBIA",
                     choices = levels(as.factor(traces$`RecoveryState`))),
      checkboxInput("exclude_state", "Exclude selected State in totals?"),
      sliderInput("years", "Years", min = 2010, max = 2017, value = c(2010, 2017), step = 1, sep = ""),
      h4("Download Full Cleaned Trace Dataset"),
      downloadButton('downloadData', 'Download'),
      # h4("Download Cato Dataset"),
      # downloadButton('downloadCato', 'Download Cato'),
      # h4("Download NCIC Dataset"),
      # downloadButton('downloadNCIC','Download NCIC'),
      h4()
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "intro",
              fluidPage(
                box(
                  htmlOutput("intro_html"),
                  width = 12
                )
              )),
      tabItem(tabName = "map",
              fluidPage(
                withMathJax(),
                plotlyOutput("source_map", height = '800px'),
                textOutput("selection"),
                helpText('A log color scale was used to make this map 
               because most of the distributions were very right tailed.
               We add 1 to the total to account for 0s.
               $$\\log(guns+1)$$'),
                h5("To export the plot, click on the camera at the top right corner.")
              )),
      tabItem(tabName = "chart",
              fluidPage(
                fluidRow(
                  plotlyOutput("source_time_chart")
                ),
                fluidRow(
                  plotlyOutput("destination_time_chart")
                ),
                h5("To export the plots, click on the camera at the top right corner on each plot.")
              )),
      tabItem(tabName = "table",
              fluidPage(fluidRow(box(h2(textOutput("table_title")), align='center'),
                                 box(downloadButton("downloadSubsetData", "Download Data Displayed"))),
                        fluidRow(box(DT::dataTableOutput("cross_tab")))
              )),
      tabItem(tabName = "cato",
              fluidPage(fluidRow(
                box(selectizeInput("cato_factor",
                                          "Select Cato Factor to Model",
                                          selected = "Gun Rights",
                                          choices = colnames(df_means))),
                box("This chart shows the relationship between various Cato Institute Indicies 
                    for 'Gun Rights' and the percentage of guns recovered in a state and traced to the same state.
                    As you see below, all the indicies have positive relationships with the fraction of guns 
                    purchased and recovered in the same state. Another way to think of that is: 
                    'The more stringent the gun laws the more guns that come from other states.'
                    It is important to note that the methodology behind the indicies is unclear at best and 
                    partisan grandstanding at worst. They apply arbitrary values for the presence of certain
                    gun laws and then take a z-score of a binomial or multi-nomial distribution they just created.")
                ),
                fluidRow(
                  box(plotOutput("cato_charts", height = "700px"), width = 12)
                )
              )),
      tabItem(tabName = "linear",
              fluidPage(box(includeMarkdown('LM_selection.md')),
                        box(includeMarkdown("LM_results.md"))
                        )),
      
      tabItem(tabName = "tree",
              fluidPage(
                fluidRow(
                  box(
                    sliderInput("cluster_k", 
                                label = "Choose Number of Clusters",
                                min = 2, max = 25, value = 2)
                    ),
                  box(selectizeInput("cato_factor2",
                                     "Select Cato Factor to Model",
                                     selected = "Gun Rights",
                                     choices = colnames(df_means)))
                  ),
                fluidRow(
                  box(plotOutput("dendrogram")),
                  box(plotOutput("clustering_chart"))
                ),
                fluidRow(
                  box("Here you can see a Hierarchical Clustering using all available CATO variables and the rate of intra-state recoveries.
                      It clearly shows that there are two distinct groups, States that have stricter guns laws and those that don't. 
                      Generally, the nine states that have the stricter gun laws (CA, CT< HI, IL, MA, MD, NJ, NY, RI) also have fewer 
                      intra-state traces meaning that a larger fraction of the guns recovered there come from states with laxer gun laws."),
                  box("The biggest takeaway from this is that the vast majority of states have very similar and permissive
                    gun laws. The few states that have tried to implement forms of gun control have had some success,
                    but have done so in widely varying legislation. It is also evident that there is a notable effect on the 
                    availability of guns, that is filled by neighboring jurisdictions.")
                )
              )),
      tabItem(tabName = "pca",
              fluidPage(
                fluidRow(
                  box(plotOutput('PCA1')),
                  box(plotOutput('PCA2'))
                  ),
                fluidRow(
                  box("The Biplot above shows that the variables share similar relationships to each other and there is a lot of multi-collinearity
                      going on here."),
                  box("The plot above shows that the bast majority of the Variance can be explained with just two or three principal components.
                      This makes sense because most of the measures are of very similar items.")
                )
                ))
    )
  )
)

# SERVER ------------------------------------------------------------------

server <- function(input, output, session){

  # observe({
  #   if(input$tab == "map" & input$source_recovery == "Source States"){
  #     updateActionButton()
  #   }
  # })
  #Create Data for Source Maps & Datatable
  source_map_data <- reactive({
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
  })
  
  recovery_map_data <- reactive({
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
  })
  
  
  
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
    filename = function() {"full_trace_data_10_17.csv"},
    content = function(fname){
      write.csv(traces, fname, row.names = F)
    }
  )
  
  output$downloadCato <- downloadHandler(
    filename = function() {"CATO Gun Rights Indicies.csv"},
    content = function(fname){
      write.csv(cato, fname, row.names = F)
    }
  )
  
  output$downloadNCIC <- downloadHandler(
    filename = function() {"NCIC_background_checks.csv"},
    content = function(fname){
      write.csv(NCIC_df, fname, row.names = F)
    }
  )
  
  #setting up html output
  getPage<-function() {
    return(includeHTML("intro_page.html"))
  }
  
  #Grabbing the output intro from .html that was written with R Markdown
  output$intro_html<-renderUI({getPage()})
  
  #Source & Recovery Maps
  output$source_map <- renderPlotly({
    if(input$source_recovery == "Source States"){
      guns <- source_map_data()
      
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
      guns <- recovery_map_data()
      
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
  
  #table title
  output$table_title <- renderText({paste(input$source_recovery, "For", 
                                          str_to_title(input$state), "guns From", 
                                          input$years[1], "To", input$years[2])})
  
  #table showing raw data
  output$cross_tab <- DT::renderDataTable({
    DT::datatable(
      if(input$source_recovery == "Source States"){
        source_map_data() %>%
          select(-lGuns)
        } else {
          recovery_map_data() %>%
            select(-lGuns)
        },
      extensions = list("Scroller", "FixedColumns", "Buttons"),
      options = list(pageLength = 56, scrollY = "800px", buttons = c('csv', 'excel'))
      )
  })
  
  #server info for download button
  output$downloadSubsetData <- downloadHandler(
    filename = function() {paste(input$source_recovery, "for", input$state, input$years[1], input$years[2],".csv",sep="_")},
    content = function(fname){
      if(input$source_recovery == "Source States"){
        guns <- source_map_data() %>% select(-lGuns)
        write.csv(guns, fname, row.names = F)
      } else{
        guns <- recovery_map_data() %>% select(-lGuns)
        write.csv(guns, fname, row.names = F)
      }
    }
  )
  
  output$cato_charts <- renderPlot({
    m <- lm(`Rate of Home Recoveries`~df_means[[input$cato_factor]], data = df_means)
    a <- signif(coef(m)[1], 2)
    b <- signif(coef(m)[2], 2)
    r2 <- signif(summary(m)$r.squared, 3)
    textlab <- paste0("y = ", b , "x + ", a, "; R^2", " = ", r2)
    
    ggplot(data = df_means, aes(x = df_means[[input$cato_factor]], y = `Rate of Home Recoveries`)) +
      geom_smooth(method = "lm") +
      geom_text(aes(label = Abbr)) +
      xlab(paste(input$cato_factor)) +
      ylab("Fraction of Recovered Guns Traced to Same Source State") +
      geom_text(aes(x = 0, y = .2, label = textlab))
      
  })
  
  output$dendrogram <- renderPlot({
    clust1 <- hclust(dist(df_means[,2:28]))
    midpoint <- (clust1$height[nrow(df_means)- input$cluster_k] + clust1$height[nrow(df_means)-input$cluster_k +1])/2
    plot(clust1)
    abline(h=midpoint, col = 'red')
  })
  
  output$clustering_chart <- renderPlot({
    clust1 <- hclust(dist(df_means[,2:28])) 
    ggplot(data = df_means, aes(x = df_means[[input$cato_factor2]], y = `Rate of Home Recoveries`)) +
      geom_point(aes(col = as.character(cutree(clust1,input$cluster_k)), size = 2)) +
      geom_text(aes(label = Abbr))+
      xlab(paste(input$cato_factor2)) +
      scale_color_discrete(name = "Clusters") + 
      scale_size_continuous(guide=F)
      
      
  })
  
  output$PCA1 <- renderPlot({
    guns_pca <- prcomp(df_means[,2:28], scale = T, center = T)
    biplot(guns_pca, cex = .5)
  })
  
  output$PCA2 <- renderPlot({
    guns_pca <- prcomp(df_means[,2:28], scale = T, center = T)
    plot(guns_pca$sdev^2/sum(guns_pca$sdev^2), xlab = "Principal Component",
         ylab = "Proportion of Variance Explained", ylim = c(0,1), type = 'b')
  })
}

# APP ---------------------------------------------------------------------

shinyApp(ui, server)