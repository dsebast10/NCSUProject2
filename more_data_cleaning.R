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
  
home_recoveries <- 
  traces %>%
  filter(SourceState == RecoveryState) %>%
  rename(HomeGuns = Guns)
  
away_recoveries <- 
  traces %>%
  filter(SourceState != RecoveryState) %>%
  group_by(RecoveryState, Year) %>%
  summarize(AwayGuns = sum(Guns))

recovery_rates <- inner_join(home_recoveries, away_recoveries, 
                             by = c("RecoveryState", "Year")) %>%
  mutate(home_away_rate = HomeGuns/AwayGuns)

hist(recovery_rates$home_away_rate)

ggplot(data = recovery_rates, aes(x = HomeGuns, y = AwayGuns)) +
  geom_point()+
  geom_smooth(method = "lm")


recoveries2NCIC <- inner_join(recovery_rates, a2, by = c("RecoveryState" = "State", "Year" = "Year")) %>%
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

library(censusapi)

Sys.setenv(CENSUS_KEY = 'f672dbc74e655222352e50077afee8c316c7ad6d')

acs <- getCensus( name = "acs/acs1", vintage = 2017, 
                  vars = c('LSAD_NAME', 'B01001_001E'),
                  region = "state:*") %>%
  mutate(State = str_to_upper(LSAD_NAME),
         POP = B01001_001E)


acs_ts <- getCensus(name = "timeseries/idb/1year", vars = c("POP", "NAME"), region = "state:*", time = 2010)

# Joins --------------------------------------------------------------------

trace_cato_rates <- inner_join(recovery_rates,
                               select(cato, State, Year, `Gun Rights`),
                               by = c("RecoveryState" = "State", "Year")) %>%
  left_join(select(acs, State, POP), by = c("RecoveryState" = "State")) %>%
  left_join(a2, by = c("RecoveryState" = "State", "Year")) %>%
  mutate(pop_adj_recov = (HomeGuns + AwayGuns)/POP)


# Plots -------------------------------------------------------------------

full_data <- left_join(cato_sources_NCIC, acs, by = c("SourceState" = "State")) %>%
  mutate(pop_adj_guns = Sourced_Guns/POP*100000,
         NCIC_adj_guns = Sourced_Guns/Total*100000)

ggplot(data = trace_cato_rates,
       aes(x = `Gun Rights`, y = home_away_rate)) +
  geom_point() +
  #geom_text (aes(label = SourceState)) +
  geom_smooth(method = "lm") #+
  facet_wrap(aes(facet = Year))

multi_lm <- lm(home_away_rate~`Gun Rights`+POP*BackgroundChecks + POP*(HomeGuns + AwayGuns),
               data = filter(trace_cato_rates, RecoveryState != "KENTUCKY"))

multi_lm
summary(multi_lm)

plot(multi_lm)