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


ggplot(data = cato_sources_NCIC, aes(x= Sourced_Guns, y = `Gun Rights`)) +
  geom_point()


# Census Data -------------------------------------------------------------

Sys.setenv(CENSUS_KEY = 'f672dbc74e655222352e50077afee8c316c7ad6d')

acs <- getCensus( name = "acs/acs1", vintage = 2017, 
                  vars = c('LSAD_NAME', 'B01001_001E'),
                  region = "state:*") %>%
  mutate(State = str_to_upper(LSAD_NAME),
         POP = B01001_001E)


acs_ts <- getCensus(name = "timeseries/idb/1year", vars = c("POP", "NAME"), region = "state:*", time = 2010)

# Plots -------------------------------------------------------------------

full_data <- left_join(cato_sources_NCIC, acs, by = c("SourceState" = "State")) %>%
  mutate(pop_adj_guns = Sourced_Guns/POP*100000,
         NCIC_adj_guns = Sourced_Guns/Total*100000)

ggplot(data = full_data %>% filter(Year == 2016),
       aes(x = `Gun Rights`, y = pop_adj_guns)) +
  geom_point() +
  #geom_text (aes(label = SourceState)) +
  geom_smooth(method = "glm") +
  facet_wrap(aes(facet = Year))


pop_rights_lm <- lm(pop_adj_guns~`Gun Rights`, data = full_data)
pop_rights_lm
summary(pop_rights_lm)


pop_rights_glm <- glm(pop_adj_guns~poly(`Gun Rights`,2), data = full_data, family = "gaussian")
pop_rights_glm
summary(pop_rights_glm)
