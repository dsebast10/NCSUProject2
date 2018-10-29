input <- list()
input$state = "DISTRICT OF COLUMBIA"
input$source_recovery = "Source State"
input$exclude_state = FALSE
input$years = c(2015, 2017)

if (input$source_recovery == "Source State"){
  df <- 
    traces %>%
    filter(Year %in% seq(input$years[1], input$years[2]),
           `Recovery State` == input$state,
           if(input$exclude_state) `Source State` != input$state else .)
} else {
  df <-
    traces %>%
    filter(Year %in% seq(input$years[1], input$years[2]),
           `Source State` == input$state,
           if(input$exclude_state) `Recovery State` != input$state else .)
}

tableize <- function(df, source_recovery, years) {
  years_list <- list()
  j <- 1
  for(i in years[1], years[2]){
    years_list[[j]] <- i
    j<-j+1
  }
  df %$%
    spread(Year, Guns) %>%
    mutate_(Total)
}

tableize(df, input$source_recovery)

test <- paste('`', input$source_recovery, '`', sep = '')
test <- "Year"

years_list <- list()
j <- 1
for(i in input$years[1]:input$years[2]){
  years_list[[j]] <- i
  j<-j+1
}
df %>%
  spread(Year, Guns) %>%
  mutate(`2016`+ `2017`)


df %>%


df %$%
  table(Year)


P <- plot_ly(
  type = ‘choropleth’,
  locations = c(‘AZ’, ‘CA’, ‘VT’),
  locationmode = ‘USA-states’,
  colorscale = ‘Viridis’, 
  z = c( 10, 20, 40 )) %>%
  layout ( geo = list ( scope = ‘usa’ ))

[]i-0k9Zr4d'0ijod
?