# Description: read code from TMS-4 files
# Creation: 2021-08-16


## Required Packages
library(tidyverse)
library(googlesheets4)

## Functions
read_tms4 <- function(file) {
  
  # Extract serial number from filename
  serial <- file %>% str_split(pattern = "_") %>% last() %>% 
    pluck(2) %>% # count from the beginning of the pieces
    tail(n=2)
  print(file)
  
  # Read the data file
  data <- read_delim(file, delim = ";",
                     col_names = F, 
                     locale=locale(decimal_mark = ",")) # check this is consistent with delim settings in Lolly
  
  
  # Check file has contents. Empty files due to bad data download only have "File is empty" as text. 
  if (ncol(data) > 1) {
    # Create vector of column names
    vars <- c("Index", "Datetime_UTC", "TimeZone", "T1", "T2", "T3", "SoilMoistureCount", "shake",
              "errFlag", "empty")
    
    # Format data for output
    names(data) <- vars
    
    data_with_ID <- data  %>% 
      mutate(SerialID = serial) %>% 
      select(SerialID, everything()) %>% 
      mutate(Datetime_UTC = lubridate::parse_date_time(Datetime_UTC,orders = c("%Y.%m.%d %H:%M")))
    
  } else {
    print("empty file")
    data_with_ID <- NULL
  }
  
  
  return(data_with_ID)
}

## Read-in data files

tomst_folder <- "data/tms-4"
files <- list.files(path = tomst_folder, pattern = "^data_*", full.names = T)

data <- map_dfr(files, read_tms4)

## Data correspondence key 

tms4_key <-read_sheet("https://docs.google.com/spreadsheets/d/1V-06dwY4Ayn1oAbOwR1RhXjjAFKdQ0i8VXjCQb6Eq9Q/edit#gid=0") %>% 
  mutate(SerialID = as.character(SerialID))

## Plot data

data_to_plot <- data %>% 
  left_join(tms4_key) %>% 
  filter(Datetime_UTC > lubridate::ymd_hm("2023-05-12 18:00")) %>% 
  filter(SerialID >95252815) %>% 
  pivot_longer(cols = T1:SoilMoistureCount,
               names_to = "Variable",
               values_to = "Value")

ggplot(data_to_plot, aes(x = Datetime_UTC, y = Value)) +
  geom_line(aes(color = Treatment)) + 
  facet_grid(Variable ~ Location, scales = "free_y") +
  theme_minimal()


## Compare OTC 

data_comp <- data %>% 
  left_join(tms4_key) %>% 
  filter(Datetime_UTC > lubridate::ymd_hm("2023-05-12 18:00")) %>% 
  filter(SerialID >95252815) %>% 
  pivot_longer(cols = T1:SoilMoistureCount,
               names_to = "Variable",
               values_to = "Value") %>% 
  filter(Variable != "SoilMoistureCount")

## effect size

temp_diff <- data_comp %>% 
  group_by(Datetime_UTC, Treatment, Variable) %>% summarize(mean_temp = mean(Value)) %>% 
  pivot_wider(names_from = Treatment, values_from = mean_temp) %>% 
  mutate(diff_temp = OTC - CONTROL) %>% 
  filter(Variable != "SoilMoistureCount")

ggplot(temp_diff, aes(x=Datetime_UTC, y=diff_temp)) + 
  geom_line() + 
  facet_wrap(vars(Variable))

## Average temperature difference
temp_diff %>% summarize(mean = mean(diff_temp, na.rm = T))

## Calculating hour
temp_diff %>% mutate(hour = hour(Datetime_UTC)) %>% 
  group_by(hour, Variable) %>% 
  summarize(average = mean(diff_temp))

temp_hour <- data_comp %>% mutate(hour = hour(Datetime_UTC)) %>% 
  group_by(hour, Treatment, Variable) %>% 
  summarize(average = mean(Value))

ggplot(temp_hour, aes(x=hour, y=average, color = Treatment)) + 
  geom_line() + 
  facet_wrap(vars(Variable))

temp_diff %>% group_by(Variable) %>% 
  summarize(mean = mean(diff_temp))
