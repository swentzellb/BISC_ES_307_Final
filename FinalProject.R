# Sage Wentzell-Brehme
# December 9, 2020
# BISC/ES 307 Final Project

# Comparing winter climate data btwn Harvard Forest and Hubbard Brook

# load libraries
library(tidyr)

# set working directory
setwd("~/Wellesley/Year 4/Bisc_307/Final/BISC_ES_307_Final")

HB_temp <- read.csv("data/HBEF_air_temp_daily_1957-2019.csv")


HB_frost <- read.txt("data/HBEF_frost_1956-2015.txt")
HB_snowcourse <- read.csv("data/HBEF_snowcourse_1956-2020.csv")
HB_snowdepth <- read.csv("data/HBEFsnowDepth_1956-2019.csv")

HF_snow <- read.csv("data/hf237-01-snow-depth.csv")
HF_temp <- read.csv("data/hf300-05-daily-temp-m.csv")

summary(HB_temp)
table(HB_temp$STA)
