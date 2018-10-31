
input <- list()
input$state = "ARIZONA"
input$exclude_state = T
input$years = c(2015, 2017)

guns <- traces %>%
  filter(RecoveryState == input$state,
         Year %in% input$years[1]:input$years[2]) %>%
  {if (input$exclude_state) filter(., SourceState != input$state) else .} %>%
  group_by(SourceState, Year) %>%
  summarize(Guns = sum(Guns)) %>%
  {if (input$exclude_state) bind_rows(.,c(SourceState = input$state, Year = 2017, Guns = 0)) else .} #%>%
  #mutate(SourceState = str_to_title(SourceState)) %>%
  filter(SourceState %in% str_to_upper(state.name)) %>%
  arrange(SourceState) %>%
  mutate(Abbr = setNames(state.abb, SourceState),
         Guns = as.numeric(Guns),
         lGuns = log(Guns))

  
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

g <- ggplot(guns, aes(x=Year, y=Guns))+
  geom_line(aes(color=SourceState))
g
