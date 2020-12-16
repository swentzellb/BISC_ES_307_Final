HB_snow <- read.csv("data/HBEF_snowcourse_1956-2020.csv")

HB_snowdepth <- read.csv("data/HBEFsnowDepth_1956-2019.csv") # don't really need to include this

#HF data?
# HF_snow <- read.csv("data/hf237-01-snow-depth.csv")
# HF_temp <- read.csv("data/hf300-05-daily-temp-m.csv")

# Clean missing data values to NA (-99 = missing)
HB_snow$snow_depth[HB_snow$snow_depth==-99] <- NA
HB_snow$swe[HB_snow$swe==-99] <- NA
HB_snow$frost_depth[HB_snow$frost_depth==-99] <- NA
HB_snow$frost_pct[HB_snow$frost_pct==-99] <- NA

summary(HB_snow)


list_of_values <- c("S", "SI2")
SI_data <- filter(diamonds, clarity %in% list_of_values)
HB_snowtest <- HB_snow %>%
  filter(Site %in% list_of_values)



ggplot(data = HB_snow)+
  geom_point(mapping=aes(x=snow_depth, y=frost_depth))

HB_snowtest <- HB_snow %>%
  group_by(Site)
summarize(Site %in% list_of_values)