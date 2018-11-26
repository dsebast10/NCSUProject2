recovery_states <- 
  traces %>%
  group_by(RecoveryState, Year) %>%
  summarize(Recovered_Guns = sum(Guns)) #%>%
  spread(key = Year, value = Recovered_Guns) 

source_states <- 
  traces %>%
  group_by(SourceState, Year) %>%
  summarize(Sourced_Guns = sum(Guns)) #%>%
spread(key = Year, value = Recovered_Guns) 
  
  
recoveries2NCIC <- inner_join(recovery_states, a2, by = c("RecoveryState" = "State", "Year" = "Year")) %>%
  filter(RecoveryState != "KENTUCKY")

plot(recoveries2NCIC$Recovered_Guns, recoveries2NCIC$Total)


ggplot(data = recoveries2NCIC, aes(x = Recovered_Guns, y = Total)) +
  geom_point(aes(col = as.character(Year)))+
  geom_smooth(method = "lm", se = F)


summary(lm(Recovered_Guns~Total, data = recoveries2NCIC))



# Sources to NCIC ---------------------------------------------------------

sources2NCIC <- inner_join(source_states, a2, by = c("SourceState" = "State", "Year")) %>%
  filter(SourceState != "KENTUCKY")

ggplot(data = sources2NCIC, aes(x = Sourced_Guns, y = Total)) +
  geom_point(aes(col = as.character(Year)))+
  geom_smooth(method = "lm", se = F)


summary(lm(Sourced_Guns~Total, data = sources2NCIC))



# Cato --------------------------------------------------------------------

cato <- read_xlsx("Data/Freedom_In_The_50_States_2018.xlsx", sheet = "Personal") %>%
  mutate(State = str_to_upper(X__1),
         Year = as.numeric(X__2))

cato_sources_NCIC <- inner_join(sources2NCIC, cato, by = c("SourceState" = "State", "Year"))


ggplot(data = cato_sources_NCIC, aes())