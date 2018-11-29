#Read In NCIC Backgrounds for Gun Sales
library(pdftools)
library(tidyverse)
library(readxl)


x <- pdf_text('https://www.fbi.gov/file-repository/nics_firearm_checks_-_month_year_by_state.pdf')
gun_names <- c('State', 'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec', 'Total')
z <- read_fwf(x[3], fwf_empty(x[3], n=50, skip = 5, col_names = gun_names), skip = 5, n_max = 55)

a <- map(x[20:2], function(b) read_fwf(b, fwf_empty(b, n=50, skip =5, col_names = gun_names), skip = 5, n_max = 55))
a1 <- map_dfc(a, function(d) select(d,Total))
colnames(a1) <- seq(1999,2017,1)
a1 <- bind_cols(a[[1]][,1], a1) %>%
  mutate(State = str_to_upper(State))
colSums(a1[,-1])
plot(colSums(a1[,-1]), xaxt = "n", ylab = "Backround Checks Run")
axis(1, at = seq(1,19), labels = seq(1999,2017))


a2 <- map_dfr(a, function(d) select(d, State, Total))%>%
  mutate(Year = rep(1999:2017, each=55),
         State = str_to_upper(State)) %>%
  rename(BackgroundChecks = Total)


