# script to read in data from enquist lab and run p-model

library(ggplot2)
library(dplyr)
library(tidyr)

## read data
# env_data_raw <- read.csv('../data/rmbl_data_climate_merge.csv')
# env_data_raw$vpdmax_kPa <- env_data_raw$vpdmax_hPa/10
# env_data_raw$vpdmin_kPa <- env_data_raw$vpdmin_hPa/10

env_data_raw <- read.csv('../data/daymet/daymet_for_optimal_traits.csv')

# create date variable
# env_data_date <- env_data_raw %>% unite(col = "date", "year", "month", sep = ".", remove = FALSE)
# env_data_date$date <- as.numeric(env_data_date$date)

# add CO2
co2_ppm <- read.csv('../data/co2_mm_mlo.csv', skip = 51)
head(co2_ppm)
env_data_co2 <- left_join(env_data_raw, co2_ppm, by = c("year", "month"))
head(env_data_co2)

colnames(env_data_co2)[10] <- 'co2_ppm'

## load model
source('../models/calc_optimal_vcmax.R')
sourceDirectory('../models/functions', modifiedOnly = FALSE)
# source('../models/LMA_revised.R')
# source('../models/n_from_gas_exchange.R')

## check model
t_seq = calc_optimal_vcmax(tg_c = seq(1, 25, 5))
plot(t_seq$vcmax ~ seq(1, 25, 5))
plot(t_seq$vcmax25 ~ seq(1, 25, 5))
plot(t_seq$lma ~ seq(1, 25, 5))

## check CRU data

# ggplot(data = env_data, aes(x = tmean_c, y = cru_tmp)) + geom_jitter()
# lm_tmp <- lm(cru_tmp ~ tmean_c, env_data)
# anova(lm_tmp)
# 
# ggplot(data = env_data, aes(x = date, y = cru_vpd)) + geom_line() +
#   geom_line(data = env_data, aes(x = date, y = vpdmax_kPa), color = "red", alpha = 0.5) +
#   geom_line(data = env_data, aes(x = date, y = vpdmin_kPa), color = "blue", alpha = 0.5)
# 
# ggplot(data = env_data, aes(x = cru_vpd, y = vpdmax_kPa)) + geom_jitter()
# lm_vpdmax <- lm(cru_vpd ~ vpdmax_kPa, env_data)
# anova(lm_vpdmax)
# 
# ggplot(data = env_data, aes(x = cru_vpd, y = vpdmin_kPa)) + geom_jitter()
# lm_vpdmin <- lm(cru_vpd ~ vpdmin_kPa, env_data)
# anova(lm_vpdmin)

# average months per year above 0 Celsius (f)
# env_data_almont <- env_data %>% dplyr::filter(site == "almont") %>% dplyr::filter(tmean_c > 0) %>% 
#   dplyr::count(year)
# f_almont <- mean(env_data_almont$n)/12
# env_data$f[env_data$site == "almont"] <- f_almont
# 
# env_data_cbt <- env_data %>% dplyr::filter(site == "cbt") %>% dplyr::filter(tmean_c > 0) %>% 
#   dplyr::count(year)
# f_cbt <- mean(env_data_cbt$n)/12
# env_data$f[env_data$site == "cbt"] <- f_cbt
# 
# env_data_pbm <- env_data %>% dplyr::filter(site == "pbm") %>% dplyr::filter(tmean_c > 0) %>% 
#   dplyr::count(year)
# f_pbm <- mean(env_data_pbm$n)/12
# env_data$f[env_data$site == "pbm"] <- f_pbm
# 
# env_data_pfeiler <- env_data %>% dplyr::filter(site == "pfeiler") %>% dplyr::filter(tmean_c > 0) %>% 
#   dplyr::count(year)
# f_pfeiler <- mean(env_data_pfeiler$n)/12
# env_data$f[env_data$site == "pfeiler"] <- f_pfeiler
# 
# env_data_road <- env_data %>% dplyr::filter(site == "road") %>% dplyr::filter(tmean_c > 0) %>% 
#   dplyr::count(year)
# f_road <- mean(env_data_road$n)/12
# env_data$f[env_data$site == "road"] <- f_road
# 
# ## add in CO2 data
# nasa_co2 = read.csv('../data/nasa_co2_ppm.csv')
# env_data_pre = env_data
# env_data = left_join(env_data_pre, nasa_co2, by = 'year')

## run model
colnames(env_data_co2)
model_output <- calc_optimal_vcmax(tg_c = env_data_co2$tave, 
                                   z = env_data_co2$elevation, 
                                   vpdo = env_data_co2$vpd,
                                   paro = env_data_co2$par,
                                   f = env_data_co2$seasonLength,
                                   cao = env_data_co2$co2_ppm)

# add model output to dataset
env_data_co2$vcmax <- model_output$vcmax
env_data_co2$jmax <- model_output$jmax
env_data_co2$vcmax25 <- model_output$vcmax25
env_data_co2$jmax25 <- model_output$jmax25
env_data_co2$lma <- model_output$lma
env_data_co2$nphoto <- model_output$nphoto
env_data_co2$nstructure <- model_output$nstructure
env_data_co2$nall <- model_output$nall

# remove months where tmean is at or below freezing
env_data_warm <- env_data_co2 %>% dplyr::filter(tave > 0)

# create site & year summary table
env_data_summary <- env_data_warm %>% dplyr::group_by(site, elevation) %>% 
  dplyr::summarize(tave_mean = mean(tave),
                   vpd_mean = mean(vpd),
                   par_mean = mean(par),
                   co2_mean = mean(co2_ppm),
                   seasonLength_mean = mean(seasonLength),
                   vcmax25_mean = mean(vcmax25),
                   jmax25_mean = mean(jmax25),
                   vcmax_mean = mean(vcmax),
                   jmax_mean = mean(jmax),
                   nall_mean = mean(nall),
                   nstructure_mean = mean(nstructure),
                   nphoto_mean = mean(nphoto),
                   lma_mean = mean(lma))

arrange(env_data_summary, elevation)

env_data_summary_year <- env_data_warm %>% dplyr::group_by(site, elevation, year) %>% 
  dplyr::summarize(tave_mean = mean(tave),
                   vpd_mean = mean(vpd),
                   par_mean = mean(par),
                   co2_mean = mean(co2_ppm),
                   seasonLength_mean = mean(seasonLength),
                   vcmax25_mean = mean(vcmax25),
                   jmax25_mean = mean(jmax25),
                   vcmax_mean = mean(vcmax),
                   jmax_mean = mean(jmax),
                   nall_mean = mean(nall),
                   nstructure_mean = mean(nstructure),
                   nphoto_mean = mean(nphoto),
                   lma_mean = mean(lma))

###############
# print output
###############
# write.csv(env_data_co2[, -c(11:14)], 'output/rmbl_optimality_output.csv', row.names = F)

###############
# some analyses
###############

## graphs

ggplot(env_data_summary_year, aes(x = year, y = vcmax_mean)) + 
  geom_line(aes(color = site))
ggplot(env_data_summary_year, aes(x = year, y = vcmax25_mean)) + 
  geom_line(aes(color = site))
ggplot(env_data_summary_year, aes(x = year, y = jmax_mean)) + 
  geom_line(aes(color = site))
ggplot(env_data_summary_year, aes(x = year, y = nstructure_mean)) + 
  geom_line(aes(color = site))
ggplot(env_data_summary_year, aes(x = year, y = nphoto_mean)) + 
  geom_line(aes(color = site))
ggplot(env_data_summary_year, aes(x = year, y = nall_mean)) + 
  geom_line(aes(color = site))
ggplot(env_data_summary_year, aes(x = year, y = lma_mean)) + 
  geom_line(aes(color = site))
