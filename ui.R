
library(shiny)
library(tidyverse)
library(shinydashboard)

ui<- dashboardPage(
  dashboardHeader(title = "ATF Crime Gun Trace Info"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction", tabName = "intro", icon = icon("comment")),
      menuItem("Map", tabName = "map", icon = icon("map")),
      menuItem("Time Chart", tabName = "chart", icon = icon("line-chart")),
      menuItem("Data Table", tabName = "table", icon = icon("table")),
      # checkboxGroupInput("years",
      #                    "Years",
      #                    c(2017:2013),
      #                    selected = c(2017:2013)),
      sliderInput("years", "Years", min = 2013, max = 2017, value = c(2013, 2017), step = 1, sep = ""),
      radioButtons("source_recovery",
                   "Choose Data to Display on Map",
                   choices = c("Where Guns Are Coming From", "Where Guns Are Going")),
      selectizeInput("state",
                    "Select Recovery State",
                    selected = "DISTRICT OF COLUMBIA",
                    choices = levels(as.factor(traces$RecoveryState))),
      checkboxInput("include_state", "Exclude selected State in totals?")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "intro",
              fluidPage(
                box("Hello")
                )
              ),
      tabItem(tabName = "map",
              fluidPage(
                fluidRow(
                  box(plotOutput("map"), width = 12)),
                fluidRow(
                  box(tableOutput("short_table"), title = "Top 5")
                  )
                )
              ),
      tabItem(tabName = "chart"),
      tabItem(tabName = "table",
              fluidPage(
                box(tableOutput("table"))
              ))
    )
  )
)
