# Script to Import Data From Multiple Sources

# FYI, for the purposes of this project, I am demonstrating many different ways 
# to import data into R. In practice, I would change all the datasets to .rds
# files and keep them in the app to improve performance/reduce storage


# Import Traces -----------------------------------------------------------

batch_import_files <- function(location) {
  # Imports and appends like excel files of data. Handles four different datasets
  #
  # Args:
  #   location: The path to the file that contains the data to be imported. The 
  #             data should be the only items in the file
  #
  # Returns datasets:
  #   df_trace: a dataframe containing all of the trace data in the directory
  
  files <- dir(location)
  
  df_trace <- data.frame()
  
  for (i in files) {
    if (grepl("trace", i, ignore.case = T)){
      tmp_trace <- 
        read_excel(paste("Data/", i, sep = ""), skip = 1) %>%
        select(-`Source State`, -starts_with("total")) %>%
        mutate(Year = as.numeric(str_extract_all(i, "[:digit:]{4}"))) %>%
        rename(SourceState = X__1) %>% 
        filter(row_number() %in% 1:55) %>%
        gather(key = RecoveryState, value = Guns, ALABAMA:WYOMING) %>%
        filter(SourceState != "TOTAL", SourceState != "TOTALS")
      df_trace <- bind_rows(df_trace, tmp_trace)
    }
  }
  df_trace
  
}
traces <- batch_import_files("Data/")


# Import NCIC Data --------------------------------------------------------

# The NCIC Data is available from the FBI in PDF format. This part of the script
# reads in that pdf as text, then parses it out to multiple datasets per year.
# We use two map() functions to make the process efficient across the multiple 
# years and pages.

NCIC_text <- pdf_text('https://www.fbi.gov/file-repository/nics_firearm_checks_-_month_year_by_state.pdf')
NCIC_names <- c('State', 'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec', 'Total')

NCIC_list <- map(NCIC_text[20:2], function(x) read_fwf(x, fwf_empty(x, n=50, skip =5, col_names = NCIC_names), skip = 5, n_max = 55))

NCIC_df <- 
  map_dfr(NCIC_list, function(x) select(x, State, Total))%>%
  mutate(Year = rep(1999:2017, each=55),
         State = str_to_upper(State)) %>%
  rename(BackgroundChecks = Total)


# Import CATO Data --------------------------------------------------------

# Importing CATO data from spreadsheet. There are several unneeded columns and
# rows, but since we are joining this data to the other datasets, we will not
# have to work too hard on filtering.

cato <- read_xlsx("Data/Freedom_In_The_50_States_2018.xlsx", sheet = "Personal") %>%
  select(X__1:`Gun Rights`) %>%
  mutate(State = str_to_upper(X__1),
         Year = as.numeric(X__2))


# State Populations -------------------------------------------------------

# This data was grabbed using the Census API in R. To avoid sharing my key and 
# requiring others to use it, I have saved it in the accompanying data file as a
# .rds file.

acs <- readRDS('Data/acs2017.Rds')

# Remove Unneccesary Intermediates ----------------------------------------

rm(NCIC_text, NCIC_names, NCIC_list)
