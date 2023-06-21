## Ruby An
## 2023-06-18

## Packages
library(tidyverse)
library(googlesheets4)

## Files 
treatment_key <- read_sheet("https://docs.google.com/spreadsheets/d/1PL6d-5xT28oVSgbeNmw1YzwA9chrVwz8SPmr5KjDAag/edit#gid=0", 
                            skip = 5, col_types = "dddcDll") %>% 
  select(Block.Plot, Treatment) %>% 
  mutate(Snow = str_detect(Treatment, "S"), 
         OTC = str_detect(Treatment, "W"),
         Nutrients = str_detect(Treatment, "N"))

raw_data <- read_sheet("https://docs.google.com/spreadsheets/d/16xVSdXF8od_3aEwJrQ4FrA0SO1amAKwxkJEBk99hdVM/edit?usp=drive_link",
                       col_types = "dccddDDcc")

# Reshaping 
clean_data <- raw_data %>% 
  filter(!is.na(NewID)) %>% 
  left_join(treatment_key) %>% 
  mutate(Species = substr(NewID, 1,1)) 

count <- clean_data %>% group_by(Species, Treatment) %>% 
  summarize(count = n())

phase_data <- clean_data %>% 
  pivot_longer(cols = "BB":"LO", 
               names_to = "Phase", 
               values_to = "Date") %>% 
  filter(!is.na(Date)) %>% 
  
  # count by date 
  ungroup() %>% 
  group_by(Treatment, Species, Phase, Date) %>% 
  summarize(phase_count = n()) %>% 
  ungroup() %>% 
  complete(Treatment, Species, Phase, Date, # complete missing dates
           fill = list(phase_count = 0)) %>% 
  
  # cumulative count by date
  group_by(Treatment, Species, Phase) %>% 
  mutate(phase_sum = cumsum(phase_count)) %>% 
  # % phase change 
  left_join(count) %>% 
  mutate(phase_percent = phase_sum/count) %>% 
  # treatment key code columns
  mutate(Snow = str_detect(Treatment, "S"), 
         OTC = str_detect(Treatment, "W"),
         Nutrients = str_detect(Treatment, "N"))
  
## VIZ ---- 

## All treatments overview
treatment_colors <- c("black","chartreuse4","deepskyblue2", "aquamarine3","magenta3","purple3", "red2","darkorange")
(phase_plot <- ggplot(phase_data, aes(x=Date, y = phase_percent, 
                                      color = Treatment, group = Treatment)) + 
   geom_point() + 
   geom_line() + 
    scale_color_manual(values = treatment_colors) + 
   facet_grid(Species ~ Phase))

## Snow removal impact on pheno
(snow_plot <- ggplot(phase_data, aes(x=Date, y = phase_percent, color = Snow, group = Treatment)) + 
  geom_point() + 
  geom_line() + 
    scale_color_manual(values = c("black", "deepskyblue2")) + 
    facet_grid(Species ~ Phase))

## Warming impact on pheno 
(otc_plot <- ggplot(phase_data, aes(x=Date, y = phase_percent, color = OTC, group = Treatment)) + 
    geom_point() + 
    geom_line() + 
    scale_color_manual(values = c("black", "red")) + 
    facet_grid(Species ~ Phase))

## Nutrient impact on pheno 
(nut_plot <- ggplot(phase_data, aes(x=Date, y = phase_percent, color = Nutrients, group = Treatment)) + 
    geom_point() + 
    geom_line() + 
    scale_color_manual(values = c("black", "chartreuse4")) + 

    facet_grid(Species ~ Phase))

