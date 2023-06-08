# reading iButton data
# 2023-04-03
# Ruby An: rubya@princeton.edu

## packages
library(tidyverse)
library(lubridate)
library(plotly)

## functions
read_ibutton_file <- function(file) {
  data <- read_csv(file, skip = 15, col_names = c("Date", "Time", "Unit", "Value")) %>% 
    mutate(filename = basename(file))
  
  return(data)
}

## files
files <- list.files("data/Lemonick/", full.names = T) 
file_subset <- files[!is.na(str_match(files, "041023"))] #c(files[2:6], files[8:11])
treatment_map <- tibble(ID = c(LETTERS[1:8], "J"), Treatment = c("CT", "OTC", "CT", "OTC", rep("EXTRA", 5)))

## data
df <- map_dfr(file_subset, ~ read_ibutton_file(.x)) %>% 
  mutate(ID = str_extract(filename, "^[^_]*")) %>% 
  mutate(date_time = lubridate::mdy_hms(str_c(Date, " ", Time))) %>% 
  left_join(treatment_map)

## viz all data
df_plot <- df #%>% filter(Treatment != "EXTRA")

df_plot

plot <- ggplot(df_plot, aes(x=date_time, y=Value, color = Treatment, group = ID)) + 
  geom_line() + 
  labs(y = "degrees Celsius")

ggplotly(plot)

## effect size

temp_diff <- df %>% mutate(round_date_time = lubridate::round_date(date_time, unit = "10 min")) %>% 
  group_by(Treatment,round_date_time) %>% summarize(mean_temp = mean(Value)) %>% 
  pivot_wider(names_from = Treatment, values_from = mean_temp) %>% 
  mutate(diff_temp = OTC - CT)

ggplot(temp_diff, aes(x=round_date_time, y=diff_temp)) + 
  geom_line()

## Average temperature difference
temp_diff %>% summarize(mean = mean(diff_temp, na.rm = T))

## Calculating hour
temp_diff %>% mutate(hour = hour(round_date_time)) %>% 
  group_by(hour) %>% 
  summarize(average = mean(C))

temp_hour <- df_plot %>% mutate(hour = hour(date_time)) %>% 
  group_by(hour, Treatment) %>% 
  summarize(average = mean(Value))

ggplot(temp_hour, aes(x=hour, y=average, color = Treatment)) + 
  geom_line()


## daily temp 
temp_data_daily <- df %>% group_by(Date, ID, Treatment) %>% 
  summarize(daily_temp = mean(Value)) 

ggplot(temp_data_daily, aes(x = Date, y = daily_temp, color = Treatment, fill = Treatment, group = ID)) + 
  geom_line(aes(group = ID)) 
  #geom_smooth(aes(color = Treatment)) 


# daily temp average difference
temp_diff %>% mutate(Date = date(round_date_time)) %>% group_by(Date) %>% summarize(Average = mean(diff_temp))


## toolik temperature data
toolik_temp <- read_csv("data/edc_1-hour_data.csv")

toolik_temp %>% group_by(hour) %>% 
  mutate(month = month(mdy(date))) %>% 
  filter(month == 7) %>% 
  summarize(average = mean(air_temp_5m)) %>% 
  ggplot(aes(x=hour, y = average)) + 
  geom_line()

ggplot(toolik_temp) + 
  geom_line(aes(x=hour, y = air_temp_5m))

