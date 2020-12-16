# Sage Wentzell-Brehme
# December 9, 2020
# BISC/ES 307 Final Project

# Comparing winter climate data btwn Harvard Forest and Hubbard Brook

# load libraries
library(tidyverse)
library(lubridate)


# set working directory
setwd("~/Wellesley/Year 4/Bisc_307/Final/BISC_ES_307_Final")

#Hubbard Brook Data
HB_temp <- read.csv("data/HBEF_air_temp_daily_1957-2019.csv")

#summarize the data
summary(HB_temp)
table(HB_temp$STA)

#assign the date column to a date variable format
HB_temp$date <- as.Date(HB_temp$date)

#add a month column
HB_temp <- mutate(HB_temp, month = format(date, "%m"), 
                    year = year(date))

#subset the data to only look at the winter months - Dec, Jan, Feb
list_of_values <- c("01", "02", "12")
HBwinter <- HB_temp %>%
  filter(month %in% list_of_values)


#filter to just the temp data from the HQ or Headquarters plot
#create a new column for winter (ex: Dec 2018-Feb 2019 = winter 2019)
HBwinter <-  HBwinter %>%
  mutate(HBwinter, winter = ifelse(month == "12", year + 1, year))%>%
  filter(STA=="HQ")

#HBwinter$winter <- as.numeric(HBwinter$winter)
#HBwinter$winter[HBwinter$month== "12"] <- HBwinter$year + 1
  #  mutate(HBwinter, winter = year)

#summarize the data by winter to look at the number of days that stay below freezing
#or have a maximum temp below 0 degrees Celsius
cold <- HBwinter %>%
  group_by(winter)%>%
  summarize(below = sum(MAX<=0))


#summarize data by winter to look at average temp and variation
#including average, maximum, and minimum daily temps
HBsum <- HBwinter %>%
  group_by(winter) %>%
  summarize(mean = mean(AVE),
            sdave = sd(AVE),
            minmean = mean(MIN), 
            sdmin = sd(MIN), 
            maxmean = mean(MAX), 
            sdmax = sd(MAX))

# plotting variation in average daily temp over each winter
ggplot(data = HBsum) + 
  geom_point(mapping = aes(x = winter, y = sdave)) +
  labs(x = "Date", y = "Variation in Average Daily Temp")

# plotting variation in minimum daily temp over each winter
ggplot(data = HBsum, mapping = aes(x = winter, y = sdmin)) + 
  geom_point() +
  geom_smooth(method=lm)+
  labs(x = "Date", y = "Variation in Minimum Daily Temp")+
  theme_bw()

#The format for the lm() function is y-variable ~ x-variable
lm1 <- lm(sdmin ~ winter, data=HBsum)
summary(lm1)

# plotting variation in max daily temp over each winter
ggplot(data = HBsum, mapping = aes(x = winter, y = sdmax)) + 
  geom_point() +
  geom_smooth(method=lm)

lm3 <- lm(sdave ~ winter, data=HBsum)
summary(lm3)
  
# plot the winter average daily minimum temp by year  
ggplot(data = HBsum, mapping = aes(x = winter, y = minmean)) + 
  geom_point()+
  geom_smooth(method=lm)+
  labs(x = "Date", y = "Average Minimum Temp (C)") +
  theme_bw()

lm1 <- lm(minmean ~ winter, data=HBsum)
summary(lm1)

# plot the number of days below freezing (0 degrees Celsius) each winter
ggplot(data = cold, mapping = aes(x = winter, y=below)) + 
  geom_point()+
  geom_smooth(method=lm)+
  labs(x = "Date", y = "# of Below Freezing Days") +
  theme_bw()

lm2 <- lm(below ~ winter, data=cold)
summary(lm2)
summary(HB_temp)

