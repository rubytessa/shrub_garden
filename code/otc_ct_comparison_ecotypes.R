#TMS 4 logger 

# packages
library(tidyverse)
library(plotly)

# functions

read_iButton_csv <- function(file) {
  
  print(file)
  
  id <- sub(".*: ", "", read_csv(file, n_max = 13, col_names = F, show_col_types = F)[2,]) # extract ibutton registration number
  
  data <- read_csv(file, skip = 15, col_names = c("Date", "Time", "Unit", "Value"), show_col_types = F) %>% 
    mutate(Date = lubridate::mdy(Date)) %>% 
    mutate(Datetime = lubridate::ymd_hms(str_c(Date, " ",Time))) %>% # convert to datetime format
    mutate(iButton_ID = id) %>% 
    mutate(filename_ID = sub('.*/(.*?)\\..*', '\\1', file)) # extract tussock ID
  

  return(data)
}

# read in file key
file_key <- read_csv("data/TussockTransplants2022 - Inventory.csv") # plot to treatment information
small_file_key <- file_key %>% select(Site, Exp, Src, Rep, Plot, OTC, iButtons) #select relevant columns

# read in data
dir <- "data/iButtons"
files <- list.files(dir,full.names = T, recursive = T)
data <- map_dfr(files, read_iButton_csv)


# combine data with plot key to one sheet
temp_data <- data %>% separate(filename_ID, c("Site", "Source", "Tussock_ID")) %>% 
  mutate(Rep = gsub("\\D", "", Tussock_ID)) %>% 
  mutate(Plot = str_c(Source, Rep)) %>% 
  select(-Rep) %>% 
  left_join(file_key) 

## write_csv(temp_data, "data/ecotypes_iButton_data.csv")

# number of plots per treatment
temp_data %>% group_by(Site, Rep, Plot, OTC) %>% 
  filter(Exp == "OTC") %>% 
  summarize(count = n()) %>% 
  group_by(Site, OTC) %>% 
  summarize(count = n())

# treatment assignment by plot 
temp_data %>% group_by(Site, Rep, Plot, OTC) %>% 
  filter(Exp == "OTC") %>% 
  summarize(count = n()) %>% 
  print(n=40)

# viz all data
plot_all_data <- ggplot(temp_data, aes(x = Datetime, y = Value, group = iButton_ID)) + 
  geom_line(aes(color = OTC)) + 
  geom_smooth(aes(color = OTC)) + 
  facet_wrap(vars(Site))

ggplotly(plot_all_data)

# calculate daily mean temp
temp_data_daily <- temp_data %>% group_by(Date, iButton_ID, Site, Plot, OTC) %>% 
  summarize(daily_temp = mean(Value)) 

ggplot(temp_data_daily, aes(x = Date, y = daily_temp, color = OTC, fill = OTC, group = OTC)) + 
  geom_line(aes(group = iButton_ID)) + 
  geom_smooth(aes(color = OTC)) + 
  facet_wrap(vars(Site)) 

# calculate site means OTC vs CT for daily temp
site_daily <- temp_data_daily %>% group_by(Date, Site, OTC) %>% 
  summarize(site_daily_temp = mean(daily_temp)) 

effect <- site_daily %>% 
  pivot_wider(names_from = OTC, values_from = site_daily_temp) %>% 
  rename(OTC = `1`, CT = `NA`) %>% 
  mutate(Effect = OTC-CT) %>% 
  filter(Site != "CF")

effect_plot <- ggplot(effect, aes(x = Date, y = Effect)) + 
  geom_line() + 
  geom_smooth() + 
  facet_wrap(vars(Site)) +
  labs(y = "OTC effect (deg C)",
       title = "Impact on Daily Mean Temp (OTC - CT)",
       subtitle = "**for TL there are OTC (n=7), CT (n=11); for SG, OTC (n=8) and CT (n=8).")

ggplotly(effect_plot)
# average effect
effect %>% group_by(Site) %>% summarize(Average = mean(Effect))

