# script to read in data from enquist lab and run p-model

library(ggplot2)
library(dplyr)
library(tidyr)

## read data
env_data <- read.csv('../data/rmbl_data_climate_merge.csv')

env_data$vpdmax_kPa <- env_data$vpdmax_hPa/10
env_data$vpdmin_kPa <- env_data$vpdmin_hPa/10

env_data <- env_data %>% unite(col = "date", "year", "month", sep = ".", remove = FALSE)
env_data$date <- as.numeric(env_data$date)

## load model
source('../p_model/calc_optimal_vcmax.R')
sourceDirectory('../p_model/functions', modifiedOnly = FALSE)

## check CRU data

ggplot(data = env_data, aes(x = tmean_c, y = cru_tmp)) + geom_jitter()
lm_tmp <- lm(cru_tmp ~ tmean_c, env_data)
anova(lm_tmp)

ggplot(data = env_data, aes(x = date, y = cru_vpd)) + geom_line() +
  geom_line(data = env_data, aes(x = date, y = vpdmax_kPa), color = "red", alpha = 0.5) +
  geom_line(data = env_data, aes(x = date, y = vpdmin_kPa), color = "blue", alpha = 0.5)

ggplot(data = env_data, aes(x = cru_vpd, y = vpdmax_kPa)) + geom_jitter()
lm_vpdmax <- lm(cru_vpd ~ vpdmax_kPa, env_data)
anova(lm_vpdmax)

ggplot(data = env_data, aes(x = cru_vpd, y = vpdmin_kPa)) + geom_jitter()
lm_vpdmin <- lm(cru_vpd ~ vpdmin_kPa, env_data)
anova(lm_vpdmin)


## run model
model_output <- calc_optimal_vcmax(tg_c = env_data$tmean_c, 
                                   z = env_data$elev, 
                                   vpdo = env_data$cru_vpd)
