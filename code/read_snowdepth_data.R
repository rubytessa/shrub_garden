## author: Ruby An
## date: 2023-05-12

## packages 
library(tidyverse)
library(googlesheets4)
library(googledrive)
library(lubridate)

## SCRATCH CODE ATTEMPTS _ KEEPING FOR FUTURE IMPROVEMENTS

# # Authenticate with Google Drive
# drive_auth()
# gs4_auth(token = drive_token())
# 
# ## ASPIRATIONAL METHOD THAT SHOULD WORK BUT IS THROWING ERRORS 
# # Get the ID of the folder you want to list the files from
# folder_id <- "https://drive.google.com/drive/u/1/folders/1YqB4OjzRfhbAMbfWUGTZHfrf0vrwncku"
# 
# # Get the list of files in the folder
# files <- drive_ls(as_id(folder_id))
# sub_files <- files %>% filter(str_detect(name, pattern ="snow_depth_garden_2023"))
# 
# data <- map_dfr(sub_files, read_sheet)
# 
# # Extract the filenames from the list of files
# filenames <- sub_files$name
# 
# read_sheet(sheets[2]) %>% print(n=25)
# 
# ## List of sheets
# 
# sheets <- c("https://docs.google.com/spreadsheets/d/1eJSJASxCUFoBsA9cVLMg3zcQDkSI390szu8d5uPPSgk/edit?usp=share_link", #2023-05-02
#             "https://docs.google.com/spreadsheets/d/1zyd8avdltcdF_9jHQkPxuZs1oIpKH-igPUDejUJ1Gio/edit?usp=share_link", #2023-05-03
#             "https://docs.google.com/spreadsheets/d/1MamtE7BcWPOWKNvuJum7KyR1E57ZUMbvd-Xt2FZ0V7o/edit?usp=share_link", #2023-05-04 
#             "https://docs.google.com/spreadsheets/d/1qe5eed2S53rPO5tSpa7QnWiFHHFHWCyrDLaJxdbq6DU/edit#gid=0", #2023-05-08
#             "https://docs.google.com/spreadsheets/d/1PXDOvu-YPAcEPxXRcDrUIDL6wVwDlqNHzFyoyxtMlZg/edit?usp=share_link") #2023-05-11
#             
# data <- map_dfr(sheets, read_sheet)

## Combined File
drive_auth()
gs4_auth(token = drive_token())

file <- "https://docs.google.com/spreadsheets/d/1f_P0UHagy12dWcko1YN3HkhcK4V0j3C_UcTPoHhVIuQ/edit#gid=0"
raw_data <- read_sheet(file)

data <- raw_data %>% pivot_longer(cols = starts_with("2023"), 
                              values_to = "depth", names_to = "date.rep", 
                              values_drop_na = T) %>% 
  separate(date.rep, c("date", "rep", "treatment"), "\\.") %>% 
  mutate(date = as_date(date))

avg_data <- data %>% group_by(Shoveled, date) %>% 
  summarize(depth = mean(depth))

ggplot(avg_data, aes(x=date, y=depth, color = Shoveled)) + 
  geom_point() + 
 geom_line() + 
  geom_point(data = data, aes(x=date, y = depth, color = Shoveled)) +
  facet_wrap(vars(Block))

## Snowdepth Menu

file <- "https://docs.google.com/spreadsheets/d/1K7gubc_H8zGxqDqaHCgvYby9tiCx6aRLwUGDS-Xl7KY/edit#gid=0"
raw_data <- read_sheet(file, skip = 7)

data <- raw_data %>% pivot_longer(cols = starts_with("2023"), 
                                  values_to = "depth", names_to = "date.rep", 
                                  values_drop_na = T) %>% 
  separate(date.rep, c("date", "rep", "treatment"), "\\.") %>% 
  mutate(date = as_date(date)) %>% 
  mutate(PlotID = Treatment) %>% 
  mutate(Treatment = str_sub(PlotID,1, 1))

avg_data <- data %>% group_by(Treatment, date) %>% 
  summarize(depth = mean(depth))

ggplot(avg_data, aes(x=date, y=depth, color = Treatment)) + 
  geom_point() + 
  geom_line() + 
  geom_point(data = data, aes(x=date, y = depth, color = Treatment))

