rm(list = ls())
setwd(getwd())
library(tidyverse)

###  snow depth ####

sn <- read.csv("imnaviat-snow-2012-2023 (1).csv")
tail(sn)
sn1 <- sn[124:nrow(sn),] ##not including fall
sn1$date2021 <- as.POSIXlt(sn1$date2021, format = "%m/%d/%y")
sn1$date2021 <- format(sn1$date2021, "%j")
sn1$date2015 <- as.POSIXlt(sn1$date2015, format = "%m/%d/%y")
sn1$date2015 <- format(sn1$date2015, "%j")

sn21 <- sn1[,c(1,2)]
sn15 <- sn1[,c(3,4)]
sn21$year <- rep(2021)
sn15$year <- rep(2015)
names(sn21) <- c("julian_day", "depth", "year")
names(sn15) <- c("julian_day", "depth", "year")
###### messy data making######
sn12 <- read.csv("snowdepth_2012.csv")
sn12$date <- as.POSIXlt(sn12$date, format = "%m/%d/%y")
sn12$julian_day <- format(sn12$date, "%j")
sn12 <- sn12[,c(3,2)]
sn12$year <- rep(2012)

sn13 <- read.csv("snowdepth_2013.csv")
sn13$date <- as.POSIXlt(sn13$date, format = "%m/%d/%y")
sn13$julian_day <- format(sn13$date, "%j")
sn13 <- sn13[,c(3,2)]
sn13$year <- rep(2013)

sn14 <- read.csv("snowdepth_2014.csv")
sn14$date <- as.POSIXlt(sn14$date, format = "%m/%d/%y")
sn14$julian_day <- format(sn14$date, "%j")
sn14 <- sn14[,c(3,2)]
sn14$year <- rep(2014)

sn16 <- read.csv("snowdepth_2016.csv")
sn16$date <- as.POSIXlt(sn16$date, format = "%m/%d/%y")
sn16$julian_day <- format(sn16$date, "%j")
sn16 <- sn16[,c(3,2)]
sn16$year <- rep(2016)

sn17 <- read.csv("snowdepth_2017.csv")
sn17$date <- as.POSIXlt(sn17$date, format = "%m/%d/%y")
sn17$julian_day <- format(sn17$date, "%j")
sn17 <- sn17[,c(3,2)]
sn17$year <- rep(2017)

sn18 <- read.csv("snowdepth_2018.csv")
sn18$date <- as.POSIXlt(sn18$date, format = "%m/%d/%y")
sn18$julian_day <- format(sn18$date, "%j")
sn18 <- sn18[,c(3,2)]
sn18$year <- rep(2018)

sn19 <- read.csv("snowdepth_2019.csv")
sn19$date <- as.POSIXlt(sn19$date, format = "%m/%d/%y")
sn19$julian_day <- format(sn19$date, "%j")
sn19 <- sn19[,c(3,2)]
sn19$year <- rep(2019)

sn20 <- read.csv("snowdepth_2020.csv")
sn20$date <- as.POSIXlt(sn20$date, format = "%m/%d/%y")
sn20$julian_day <- format(sn20$date, "%j")
sn20 <- sn20[,c(3,2)]
sn20$year <- rep(2020)

sn22 <- read.csv("snowdepth_2022.csv")
sn22$date <- as.POSIXlt(sn22$date, format = "%m/%d/%y")
sn22$julian_day <- format(sn22$date, "%j")
sn22 <- sn22[,c(3,2)]
sn22$year <- rep(2022)

sn23 <- read.csv("snow_depth2023.csv")
sn23$date <- as.POSIXlt(sn23$date, format = "%m/%d/%y")
sn23$julian_day <- format(sn23$date, "%j")
sn23 <- sn23[,c(3,2)]
sn23$year <- rep(2023)
##### snow figure ####

snow <- rbind(sn12, sn13, sn14, sn15, sn16, sn17, sn18, sn19, sn20, sn21, sn22, sn23)
snow$year <- as.factor(snow$year)
snow$julian_day <- as.numeric(snow$julian_day)
#clrs <- c("gray", "gray", "gray", "gray", "gray", "gray", "gray", "gray", "blue", "red", "darkgreen")
snow$year <- factor(snow$year, levels = c("2012", "2013", "2014", "2016", "2017", "2018", "2019", "2020", "2015", "2021", "2022", "2023"))
write.csv(snow, "imnaviat-snow-2012-2023.csv", row.names = F)
library(gghighlight)

### RUBY START HERE ######
library(ggpubr)
library(tidyverse)
library(lubridate)
library(gghighlight)
setwd("Projects/shrub_experiment/data")
snow <- read.csv("imnaviat-snow-2012-2023.csv")
# add recent snow
snow_recent <- read_csv("snow_2023.csv", skip = 3, col_names = c("Site", "Date", "Time", "Precip", "Temp", "Temp_max", "Temp_min", "Temp_avg", "depth"))  %>% 
  select(Date, depth) %>% 
  mutate(julian_day = yday(mdy(Date))) %>% 
  mutate(year = lubridate::year(mdy(Date))) %>% 
  select(-Date)

snow <- filter(snow, depth > -99.9) %>% bind_rows(snow_recent)
tail(snow)

ggplot(snow, aes(x = julian_day, y = depth*2.54, color = factor(year))) +
  geom_line(linewidth = 1.2) +
  #scale_color_manual(values = clrs)  +
  theme_bw() +
  scale_x_continuous(limits = c(0,210)) +
  scale_y_continuous(limits = c(-10,125)) +
  # geom_vline(xintercept = 142, linetype = 2, color = "red") +
  # annotate(geom = "text", x = 100, y = 10, label = "2015 snow free date:\n May 22", color = "red") +
  # geom_vline(xintercept = 159, linetype = 2, color = "blue") +
  # annotate(geom = "text", x = 190, y = 50, label = "2021 snow free date:\n June 8", color = "blue") +
  ylab("Snow Depth (cm)") +
  xlab("Day of Year") +
  gghighlight(year %in% c(2017:2023), label_key = category)


#snowmelt date
snow_melt <- snow %>% group_by(year) %>% 
  filter(depth == 0) %>% 
  arrange(year, julian_day) %>% 
  slice(1)

snow_melt %>% ungroup() %>% 
  summarize(avg_melt = mean(julian_day), min_melt = min(julian_day), max_melt = max(julian_day))

#### temp ####
t12 <- read.csv("temp_2012.csv")
t12$date <- as.POSIXlt(t12$date, format = "%m/%d/%y")
t12$julian_day <- format(t12$date, "%j")
t12 <- t12[,c(3,2)]
t12$year <- rep(2012)

t13 <- read.csv("temp_2013.csv")
t13$date <- as.POSIXlt(t13$date, format = "%m/%d/%y")
t13$julian_day <- format(t13$date, "%j")
t13 <- t13[,c(3,2)]
t13$year <- rep(2013)

t14 <- read.csv("temp_2014.csv")
t14$date <- as.POSIXlt(t14$date, format = "%m/%d/%y")
t14$julian_day <- format(t14$date, "%j")
t14 <- t14[,c(3,2)]
t14$year <- rep(2014)

t15 <- read.csv("temp_2015.csv")
t15$date <- as.POSIXlt(t15$date, format = "%m/%d/%y")
t15$julian_day <- format(t15$date, "%j")
t15 <- t15[,c(3,2)]
t15$year <- rep(2015)

t16 <- read.csv("temp_2016.csv")
t16$date <- as.POSIXlt(t16$date, format = "%m/%d/%y")
t16$julian_day <- format(t16$date, "%j")
t16 <- t16[,c(3,2)]
t16$year <- rep(2016)

t17 <- read.csv("temp_2017.csv")
t17$date <- as.POSIXlt(t17$date, format = "%m/%d/%y")
t17$julian_day <- format(t17$date, "%j")
t17 <- t17[,c(3,2)]
t17$year <- rep(2017)

t18 <- read.csv("temp_2018.csv")
t18$date <- as.POSIXlt(t18$date, format = "%m/%d/%y")
t18$julian_day <- format(t18$date, "%j")
t18 <- t18[,c(3,2)]
t18$year <- rep(2018)

t19 <- read.csv("temp_2019.csv")
t19$date <- as.POSIXlt(t19$date, format = "%m/%d/%y")
t19$julian_day <- format(t19$date, "%j")
t19 <- t19[,c(3,2)]
t19$year <- rep(2019)

t20 <- read.csv("temp_2020.csv")
t20$date <- as.POSIXlt(t20$date, format = "%m/%d/%y")
t20$julian_day <- format(t20$date, "%j")
t20 <- t20[,c(3,2)]
t20$year <- rep(2020)

t21 <- read.csv("temp_2021.csv")
t21$date <- as.POSIXlt(t21$date, format = "%m/%d/%y")
t21$julian_day <- format(t21$date, "%j")
t21 <- t21[,c(3,2)]
t21$year <- rep(2021)

t22 <- read.csv("temp_2022.csv")
t22$date <- as.POSIXlt(t22$date, format = "%m/%d/%y")
t22$julian_day <- format(t22$date, "%j")
t22 <- t22[,c(3,2)]
t22$year <- rep(2022)

temp <- rbind(t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22)
#temp$year <- as.factor(temp$year)
temp$julian_day <- as.numeric(temp$julian_day)

clrs <- c("gray", "gray", "gray", "gray", "gray", "gray", "gray", "gray", "blue", "red")
temp$year <- as.numeric(temp$year)
### temp figure ####
ggplot(temp, aes(x = julian_day, y = tempC, color = as.factor(year))) +
  geom_line(size = 1.7) +
  #scale_color_manual(values = c("red", "blue"))  +
  theme_bw() +
  #scale_color_manual(values = clrs) +
  scale_x_continuous(limits = c(105,210)) +
  scale_y_continuous(limits = c(-22,20)) +
  # geom_vline(xintercept = 142, linetype = 2, color = "red") +
  # annotate(geom = "text", x = 100, y = 10, label = "2015 snow free date:\n May 22", color = "red") +
  # geom_vline(xintercept = 159, linetype = 2, color = "blue") +
  # annotate(geom = "text", x = 190, y = 50, label = "2021 snow free date:\n June 8", color = "blue") +
  ylab("Ave. air temp (ºC)") +
  xlab("Day of Year") +
  geom_hline(yintercept = 0) +
  gghighlight(year == 2015 | year == 2021 | year == 2022)



m12 <- read.csv("moist_2012.csv")
m12$date <- as.POSIXlt(m12$date, format = "%m/%d/%y")
m12$julian_day <- format(m12$date, "%j")
m12 <- m12[,c(3,2)]
m12$year <- rep(2012)

m13 <- read.csv("moist_2013.csv")
m13$date <- as.POSIXlt(m13$date, format = "%m/%d/%y")
m13$julian_day <- format(m13$date, "%j")
m13 <- m13[,c(3,2)]
m13$year <- rep(2013)

m14 <- read.csv("moist_2014.csv")
m14$date <- as.POSIXlt(m14$date, format = "%m/%d/%y")
m14$julian_day <- format(m14$date, "%j")
m14 <- m14[,c(3,2)]
m14$year <- rep(2014)

m15 <- read.csv("moist_2015.csv")
m15$date <- as.POSIXlt(m15$date, format = "%m/%d/%y")
m15$julian_day <- format(m15$date, "%j")
m15 <- m15[,c(3,2)]
m15$year <- rep(2015)

m16 <- read.csv("moist_2016.csv")
m16$date <- as.POSIXlt(m16$date, format = "%m/%d/%y")
m16$julian_day <- format(m16$date, "%j")
m16 <- m16[,c(3,2)]
m16$year <- rep(2016)

m17 <- read.csv("moist_2017.csv")
m17$date <- as.POSIXlt(m17$date, format = "%m/%d/%y")
m17$julian_day <- format(m17$date, "%j")
m17 <- m17[,c(3,2)]
m17$year <- rep(2017)

m18 <- read.csv("moist_2018.csv")
m18$date <- as.POSIXlt(m18$date, format = "%m/%d/%y")
m18$julian_day <- format(m18$date, "%j")
m18 <- m18[,c(3,2)]
m18$year <- rep(2018)

m19 <- read.csv("moist_2019.csv")
m19$date <- as.POSIXlt(m19$date, format = "%m/%d/%y")
m19$julian_day <- format(m19$date, "%j")
m19 <- m19[,c(3,2)]
m19$year <- rep(2019)

m20 <- read.csv("moist_2020.csv")
m20$date <- as.POSIXlt(m20$date, format = "%m/%d/%y")
m20$julian_day <- format(m20$date, "%j")
m20 <- m20[,c(3,2)]
m20$year <- rep(2020)

m21 <- read.csv("moist_2021.csv")
m21$date <- as.POSIXlt(m21$date, format = "%m/%d/%y")
m21$julian_day <- format(m21$date, "%j")
m21 <- m21[,c(3,2)]
m21$year <- rep(2021)

moist <- rbind(m12, m13, m14, m15, m16, m17, m18, m19, m20, m21)
#moist$year <- as.factor(moist$year)
moist$julian_day <- as.numeric(moist$julian_day)
moist$year <- factor(moist$year, levels = c("2012", "2013", "2014", "2016", "2017", "2018", "2019", "2020", "2015", "2021"))
moist$soil_moisture <- ifelse(moist$soil_moisture == -99.9, NA, moist$soil_moisture)

ggplot(moist, aes(x = julian_day, y = soil_moisture, color = year)) +
  geom_line(size = 1.7) +
  #scale_color_manual(values = c("red", "blue"))  +
  theme_bw() +
  #scale_color_manual(values = clrs) +
  scale_x_continuous(limits = c(105,210)) +
  #scale_y_continuous(limits = c(-22,20)) +
  # geom_vline(xintercept = 142, linetype = 2, color = "red") +
  # annotate(geom = "text", x = 100, y = 10, label = "2015 snow free date:\n May 22", color = "red") +
  # geom_vline(xintercept = 159, linetype = 2, color = "blue") +
  # annotate(geom = "text", x = 190, y = 50, label = "2021 snow free date:\n June 8", color = "blue") +
  ylab("Ave. soil moisture") +
  xlab("Day of Year") +
  geom_hline(yintercept = 0) +
  gghighlight(year == 2015 | year == 2021, label_key = category)

tempplot
snowplot
moistplot
ggarrange(tempplot, snowplot, moistplot, ncol = 1, common.legend = TRUE, legend = "right")

## in 2015, above freezing from May 7th through May 31st
## then below freezing for 5 days
## this is when all the snow melts
## ask Jim for NDVI data?

##takeaways: 

## 2015 had earlier warm temperatures and earlier snowmelt by almost three weeks
## my data are ~5x lower than Case's
## Ed's model requires T, PAR, and LAI
## T and PAR are the same (ish) between years -- so LAI (or something unmeasured) must be the difference
## what? maybe leaf thickness/leaf N? Plant community hasn't dramatically shifted since 2015
## timing of snowmelt/warm temps in early season when light is abundant could determine seasonal carbon gain
## could also be soil moisture and/or rainfall?

#####

## NDVI
ndvi <- read.csv("2007-2019_GS_ReflectanceIndices.csv")

ndvi1 <- ndvi %>%
  filter(EXPERIMENT == "06MAT" | EXPERIMENT == "LMAT") %>%
  filter(YEAR > 2011)
unique(ndvi1$YEAR)  
str(ndvi1)


ndvi15 <- ndvi1 %>%
  filter(YEAR == 2015) %>%
  group_by(TREATMENT, YEAR, DOY, BLOCK) %>%
  summarize(mean_ndvi = mean(NDVI..MODIS.)) %>%
  filter(TREATMENT == "CT" | TREATMENT == "F0.5" | TREATMENT == "F1" | TREATMENT == "F2" | TREATMENT == "F5" | TREATMENT == "F10")
ndvi2 <- ndvi1 %>%
  group_by(TREATMENT, YEAR, DOY, BLOCK) %>%
  summarize(mean_ndvi = mean(NDVI..MODIS.)) %>%
  filter(TREATMENT == "CT" | TREATMENT == "F0.5" | TREATMENT == "F1" | TREATMENT == "F2" | TREATMENT == "F5" | TREATMENT == "F10")

ndvi15$LAI <- 0.0026*exp(8.0783*ndvi15$mean_ndvi)

ndvi2021 <- read.csv("ndvi_2021.csv")
str(ndvi2021)
ndvi2021$YEAR <- rep(2021)
names(ndvi2021)
names(ndvi2021) <- c("DateTime", "Site", "BLOCK", "TREATMENT", 
                     "replicate", "file_num", "NDVI", "EVI", 
                     "EVI2", "Year", "DOY", "YEAR")


ndvi21 <- ndvi2021 %>%
  group_by(TREATMENT, YEAR, DOY, BLOCK) %>%
  summarize(mean_ndvi = mean(NDVI)) %>%
  filter(TREATMENT == "CT" | TREATMENT == "F0.5" | TREATMENT == "F1" | TREATMENT == "F2" | TREATMENT == "F5" | TREATMENT == "F10")
names(ndvi21)
names(ndvi15)
ndvi21$LAI <- 0.0026*exp(8.0783*ndvi21$mean_ndvi)
ndvi2$LAI <- 0.0026*exp(8.0783*ndvi2$mean_ndvi)

ndvi <- rbind(ndvi15, ndvi21)
ggplot(ndvi21, aes(x = DOY, y = mean_ndvi, color = TREATMENT)) +
  geom_point() +
  theme_bw() +
  geom_smooth(method = "loess", se = F)
ndvi$TREATMENT <- factor(ndvi$TREATMENT, levels = c("CT", "F0.5", "F1", "F2", "F5", "F10"))
ndvi2$TREATMENT <- factor(ndvi2$TREATMENT, levels = c("CT", "F0.5", "F1", "F2", "F5", "F10"))

ggplot(ndvi, aes(x = DOY, y = LAI, color = as.factor(YEAR))) +
  geom_point() +
  theme_bw() +
  facet_wrap(~TREATMENT, nrow = 2) +
  geom_smooth(aes(group = as.factor(YEAR)), method = "lm", formula = y ~ poly(x,2), se = T) 

