
library(shiny)
library(tidyverse)
library(shinydashboard)

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
      tabItem(tabName = "map",
              fluidPage(
                box(plotOutput("map"))
              )),
      tabItem(tabName = "chart",
              box(textOutput("text2"))),
      tabItem(tabName = "table",
              fluidPage(
                box(tableOutput("table"))
              ))
    )
  )
)
