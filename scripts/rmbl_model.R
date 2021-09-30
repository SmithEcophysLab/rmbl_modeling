# script to read in data from enquist lab and run p-model

library(ggplot2)
library(dplyr)
library(tidyr)

## read data
env_data_raw <- read.csv('../data/rmbl_data_climate_merge.csv')

env_data_raw$vpdmax_kPa <- env_data_raw$vpdmax_hPa/10
env_data_raw$vpdmin_kPa <- env_data_raw$vpdmin_hPa/10

# create date variable
env_data_date <- env_data_raw %>% unite(col = "date", "year", "month", sep = ".", remove = FALSE)
env_data_date$date <- as.numeric(env_data_date$date)

# add CO2
co2_ppm <- read.csv('../data/nasa_co2_ppm.csv')
env_data_co2 <- left_join(env_data_date, co2_ppm, by = "year")

# remove years 2016-2019 (no CO2/CRU data)
env_data <- env_data_co2 %>% dplyr::filter(year < 2012)

## load model
source('../models/calc_optimal_vcmax.R')
sourceDirectory('../models/functions', modifiedOnly = FALSE)
source('../models/LMA_revised.R')
source('../models/n_from_gas_exchange.R')

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
                                   vpdo = env_data$cru_vpd,
                                   paro = env_data$cru_par)

# add model output to dataset
env_data$mc <- model_output$mc
env_data$vcmax <- model_output$vcmax
env_data$jmax <- model_output$jmax

# average months per year above 0 Celsius (f)
env_data_almont <- env_data %>% dplyr::filter(site == "almont")
env_data_almont %>% dplyr::count(year)
env_data_warm <- env_data_almont %>% dplyr::filter(tmean_c > 0)
env_data_f <- env_data_warm %>% dplyr::count(year)
f <- mean(env_data_f$n)/12

# estimate LMA
env_data$lma <- calc_lma(f = f, # fraction of time in growing season
                         par = env_data$cru_par, # light absorbed in µmol m-2 s-1
                         temperature = env_data$tmean_c, # temperature in C
                         vpd_kpa = env_data$cru_vpd, # vpd in kpa
                         z = env_data$elev_m, # elevation in m
                         co2 = 400, # co2 in ppm
)

# add predicted vcmax to dataset
env_data$vcmax25 <- model_output$vcmax / 
  calc_tresp_mult(model_output$tg_c, model_output$tg_c, 25)

# add predicted jmax for C3 plants to leaf dataset
env_data$jmax25 <- model_output$jmax / 
  calc_jmax_tresp_mult(model_output$tg_c, model_output$tg_c, 25)

# set predicted vpmax to zero
env_data$vpmax25 <- 0

# calculate gross photosynthesis
env_data$grossphoto <- env_data$vcmax * env_data$mc

# calculate respiration
env_data$resp <- 0.15 * env_data$vcmax

# calculate net photosynthesis
env_data$netphoto <- env_data$grossphoto - env_data$resp

# calculate leaf N in rubisco from predicted vcmax
env_data$nrubisco <- fvcmax25_nrubisco(env_data$vcmax25)

# calculate leaf N in bioenergetics from predicted jmax
env_data$nbioe <- fjmax25_nbioe(env_data$jmax25)

# calculate leaf N in rubisco from predicted vpmax with PEP-specific constants
env_data$npep <- fvpmax25_npep(env_data$vpmax25)

# calculate nitrogen in structural tissue from lma 
env_data$nstructure <- flma_nstructure(env_data$lma)

# sum all leaf N predictions
env_data$nall <- env_data$nrubisco + env_data$nbioe + env_data$nstructure + env_data$npep

# calculate leaf N used for photosynthesis
env_data$nphoto <- env_data$nrubisco + env_data$nbioe + env_data$npep

# calculate the fraction of leaf N in rubisco out of all leaf N
env_data$nrubisco_frac <- env_data$nrubisco / env_data$nall

# calculate the fraction of leaf N for photosynthesis out of all leaf N
env_data$nphoto_frac <- env_data$nphoto / env_data$nall


env_data$lognphoto <- log(env_data$nphoto)
env_data$lognstructure <- log(env_data$nstructure)

# remove months where tmean is at or below freezing
env_data_warm <- env_data %>% dplyr::filter(tmean_c > 0)

# create site & year summary table
env_data_summary <- env_data_warm %>% dplyr::group_by(site, year) %>% 
  dplyr::summarize(vcmax_mean = mean(vcmax),
                   jmax_mean = mean(jmax),
                   grossphoto_mean = mean(grossphoto),
                   netphoto_mean = mean(netphoto))

## graphs

ggplot(env_data_summary, aes(x = year, y = vcmax_mean)) + geom_line(aes(color = site))
ggplot(env_data_summary, aes(x = year, y = jmax_mean)) + geom_line(aes(color = site))
ggplot(env_data_summary, aes(x = year, y = grossphoto_mean)) + geom_line(aes(color = site))
ggplot(env_data_summary, aes(x = year, y = netphoto_mean)) + geom_line(aes(color = site))

