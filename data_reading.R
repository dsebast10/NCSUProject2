sources <-
  traces %>%
  filter(`Recovery State` == input$state,
         Year %in% seq(input$years[1], input$years[2])) %>%
         {if(input$exclude_state) filter(.$`Source State` != input$state) else .}


sources <-
  traces %>%
  filter(`Recovery State` == input$state,
         Year %in% seq(input$years[1], input$years[2])) %>%
  purrr::when(input$exclude_state ~ filter(., `Source State` != input$state), ~.)

recoveries <- 
  traces %>%
  filter(`Source State` == input$state,
         Year %in% seq(input$years[1], input$years[2])) %>%
  purrr::when(input$exclude_state ~ filter(., `Recovery State` != input$state), ~.)


test <- 
  traces %>%
  filter(UQ(rlang::sym(input$source_recovery)) == input$state,
         Year %in% seq(input$years[1], input$years[2]))
  purrr::when(((input$exclude_state & UQ(rlang::sym(input$source_recovery)) == "Source State") 
               ~ filter(., `Recovery State` != input$state)),
              ((input$exclude_state & UQ(rlang::sym(input$source_recovery)) == "Recovery State")
               ~ filter(., `Source State` != input$state)),
              ~ .)


test <- 
  traces %>%
  filter(UQ(rlang::sym(input$source_recovery)) == input$state,
         Year %in% seq(input$years[1], input$years[2]))

  

  input <- list()

input$state = 'TEXAS'
input$years = c(2016, 2017)
input$exclude_state = F
input$source_recovery = "Recovery State"

rm(input)
?mutate_
  