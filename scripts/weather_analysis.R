library(dplyr)
library(tidyr)
library(data.table)
library(stringr)
library(lubridate)
library(ggplot2)

calc_vp = function(dew_temp){
  
  e_kPa <- 0.611 * exp(7.5 * dew_temp / (237.3 + dew_temp))
  
  # es <- 0.611 * exp(7.5 * temp / (237.3 + temp))
  
  e <- e_kPa * 1000

  return(e)
}

import_csvs <- function(filepath){
  # read file path
  paths <- list.files(path = filepath, pattern = "*.csv", full.names = T)
  # read file content
  content <- paths %>% lapply(read.csv, header = T)
  # read file name
  filenames <- paths %>% basename() %>% as.list()
  # combine file content list and file name list
  lists <- mapply(c, content, filenames, SIMPLIFY = FALSE)
  # unlist all lists and change column name
  df <- rbindlist(lists, fill = T)
  return(df)
}

NEON_weather <- read.csv("../data/daily_neon_weather.csv")
NEON_weather$date <- as.Date(str_c(NEON_weather$year, NEON_weather$month, NEON_weather$day, sep = "-"), format = "%Y-%m-%d")
NEON_weather$doy <- yday(NEON_weather$date)
NEON_weather$vp <- calc_vp(NEON_weather$wssDewTempMean)

model_weather <- import_csvs(filepath = "../data/Model_weather") %>%
  separate(col = "V1", into = c("site"), sep = "[[:punct:]]")

model_weather$year <- floor(model_weather$year_mean)
model_weather$doy <- floor(model_weather$yday_mean)

weather <- full_join(model_weather, NEON_weather, by = c("site", "year", "doy"))

lm_tmax <- lm(tmax.C. ~ wssTempTripleMaximum, weather)
summary(lm_tmax)
ggplot(weather, aes(x = tmax.C., y = wssTempTripleMaximum)) + geom_point() + geom_smooth(model = lm) +
  geom_abline(intercept = 0, slope = 1)

lm_tmin <- lm(tmin.C. ~ wssTempTripleMinimum, weather)
summary(lm_tmin)
ggplot(weather, aes(x = tmin.C., y = wssTempTripleMinimum)) + geom_point() + geom_smooth(model = lm) +
  geom_abline(intercept = 0, slope = 1)

lm_vp <- lm(vp.Pa. ~ vp, weather)
summary(lm_vp)
ggplot(weather, aes(x = vp.Pa., y = vp)) + geom_point() + geom_smooth(model = lm) +
  geom_abline(intercept = 0, slope = 1)

lm_srad <- lm(srad.W.m2. ~ wssShortRadMean, weather)
summary(lm_srad)
ggplot(weather, aes(x = srad.W.m2., y = wssShortRadMean)) + geom_point() + geom_smooth(model = lm) +
  geom_abline(intercept = 0, slope = 1)

