# reading iButton data
# 2023-04-03
# Ruby An: rubya@princeton.edu

## packages
library(tidyverse)
library(lubridate)
library(plotly)
library(googlesheets4)

## functions
treatment_colors <- c("black","chartreuse4","deepskyblue2", "aquamarine3","magenta3","purple3", "red2","darkorange")

read_ibutton_file <- function(file) {
  data <- read_csv(file, skip = 15, col_names = c("Date", "Time", "Unit", "Value")) %>% 
    mutate(filename = basename(file))
  
  return(data)
}

# ecotypes comparison ----
## files ----
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

## effect size -----

temp_diff <- df %>% mutate(round_date_time = lubridate::round_date(date_time, unit = "10 min")) %>% 
  group_by(Treatment,round_date_time) %>% summarize(mean_temp = mean(Value)) %>% 
  pivot_wider(names_from = Treatment, values_from = mean_temp) %>% 
  mutate(diff_temp = OTC - CT)

ggplot(temp_diff, aes(x=round_date_time, y=diff_temp)) + 
  geom_line()

## Average temperature difference
temp_diff %>% summarize(mean = mean(diff_temp, na.rm = T))

## Calculating hour---
temp_diff %>% mutate(hour = hour(round_date_time)) %>% 
  group_by(hour) %>% 
  summarize(average = mean(C))

temp_hour <- df_plot %>% mutate(hour = hour(date_time)) %>% 
  group_by(hour, Treatment) %>% 
  summarize(average = mean(Value))

ggplot(temp_hour, aes(x=hour, y=average, color = Treatment)) + 
  geom_line()


## daily temp ------
temp_data_daily <- df %>% group_by(Date, ID, Treatment) %>% 
  summarize(daily_temp = mean(Value)) 

ggplot(temp_data_daily, aes(x = Date, y = daily_temp, color = Treatment, fill = Treatment, group = ID)) + 
  geom_line(aes(group = ID)) 
  #geom_smooth(aes(color = Treatment)) 


## daily temp average difference -----
temp_diff %>% mutate(Date = date(round_date_time)) %>% group_by(Date) %>% summarize(Average = mean(diff_temp))


# Toolik Met temperature data ----
toolik_temp <- read_csv("data/edc_1-hour_data.csv")

toolik_temp %>% group_by(hour) %>% 
  mutate(month = month(mdy(date))) %>% 
  filter(month == 7) %>% 
  summarize(average = mean(air_temp_5m)) %>% 
  ggplot(aes(x=hour, y = average)) + 
  geom_line()

ggplot(toolik_temp) + 
  geom_line(aes(x=hour, y = air_temp_5m))

# Shruburbia iButton dataSeptember 2022 - July 2023 -----

## files ----

## treatment key
key_2023 <- read_sheet("https://docs.google.com/spreadsheets/d/1zU4guAY811A76XViAZ2lWBkWBzCscL1jZfUw0I-pl6U/edit#gid=1535841976",
                        skip = 2) %>% 
  mutate(ID = str_sub(serialID, -6)) 

## data
files <- list.files("data/2023-07-06_ibuttons", full.names = T) 
file_subset <- files #files[!is.na(str_match(files, "070623"))] #c(files[2:6], files[8:11])

## data
df <- map_dfr(file_subset, ~ read_ibutton_file(.x)) %>% 
  mutate(date_time = lubridate::mdy_hms(str_c(Date, " ", Time))) %>% 
  separate(filename, c("Block.Plot", "SerialID", "Date"), sep ="_") %>% 
  mutate(Date = lubridate::date(date_time)) %>% 
  mutate(ID = str_sub(SerialID, -6)) 

data <- df %>% left_join(key_2023) %>% filter(!is.na(Treatment))

## viz all data
df_plot <- data %>% 
  mutate(Snow = str_detect(Treatment, "S"), 
         OTC = str_detect(Treatment, "W"),
         Nutrients = str_detect(Treatment, "N")) %>% 
  select(block, plot, Treatment, date_time, ID, serialID, Time, Unit, Value, 
         Snow, OTC, Nutrients) %>% 
  filter(date_time < "2023-07-05") # extraction date

(plot <- ggplot(df_plot, aes(x=date_time, y=Value, 
                             color = Treatment, group = ID)) + 
  geom_line() + 
    scale_color_manual(values=treatment_colors) +
  labs(y = "degrees Celsius") )

ggplotly(plot)
 
## effect size -----

temp_diff <- data %>% mutate(round_date_time = lubridate::round_date(date_time, unit = "4 h")) %>% 
  group_by(Treatment,round_date_time) %>% summarize(mean_temp = mean(Value)) %>% 
  pivot_wider(names_from = Treatment, values_from = mean_temp) %>% 
  mutate(diff_temp = W - C)

temp_diff_plot <- ggplot(temp_diff, aes(x=round_date_time, y=diff_temp)) + 
  geom_line()

ggplotly(temp_diff_plot)
## Average temperature difference

temp_diff %>% filter(round_date_time > "2023-05-01") %>% 
  summarize(mean = mean(diff_temp, na.rm = T))

## Calculating hour---
temp_diff %>% mutate(hour = hour(round_date_time)) %>% 
  group_by(hour) %>% 
  filter(round_date_time > "2023-06-01") %>% 
  summarize(average = mean(C))

temp_hour <- df_plot %>% mutate(hour = hour(date_time)) %>% 
  group_by(hour, Treatment) %>% 
  filter(date_time > "2023-06-01") %>% 
  summarize(average = mean(Value))

ggplot(temp_hour, aes(x=hour, y=average, color = Treatment)) + 
  geom_line() + 
  scale_color_manual(values = treatment_colors)


## daily temp ------
temp_data_daily <- data %>% group_by(Date, ID, Treatment) %>% 
  summarize(daily_temp = mean(Value)) %>% 
  filter(Date > "2023-05-01") %>% 
  filter(Date < "2023-07-05")

ggplot(temp_data_daily, aes(x = Date, y = daily_temp, color = Treatment, fill = Treatment, group = ID)) + 
  geom_line(aes(group = ID))+
  scale_color_manual(values=treatment_colors)
#geom_smooth(aes(color = Treatment)) 


## daily temp average difference -----
temp_diff %>% mutate(Date = date(round_date_time)) %>% 
  group_by(Date) %>% summarize(Average = mean(diff_temp)) %>% 
  print(n=365)



