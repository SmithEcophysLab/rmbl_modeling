# script to read in data from enquist lab and run p-model

## read data
<<<<<<< HEAD
env_data <- readRDS('../data/environmentaldata.monthly.rds')

env_data$vpdmax_kPa <- env_data$vpdmax_hPa/10
env_data$vpdmin_kPa <- env_data$vpdmin_hPa/10
env_data$vpdmean_kPa <- (env_data$vpdmax_kPa + env_data$vpdmin_kPa)/2

## load functions
source('../p_model/calc_optimal_vcmax.R')
sourceDirectory('../p_model/functions', modifiedOnly = FALSE)

model_output <- calc_optimal_vcmax(tg_c = env_data$tmean_c, 
                                   z = env_data$elev, 
                                   vpdo = env_data$vpdmean_kPa)
=======
env_data <- read.csv('../data/rmbl_data_climate_merge.csv')
head(env_data)
tail(env_data)

## load model

## run model
>>>>>>> 447a9c8fe8596566393b38e514ecd68e06715af7
