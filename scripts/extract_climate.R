# extract climate for RMBL model

##libraries
library(tidyr)
library(dplyr)
library(R.matlab)

## read data
rmbl_data_raw <- readRDS('../data/environmentaldata.monthly.rds')
head(rmbl_data_raw)
tail(rmbl_data_raw)

## read lat lon data
rmbl_latlon <- read.csv('../data/site_corrdinates.csv')

## read in climate data
cru_alpha_1901 = as.data.frame(readMat('~/Documents/Data/CRU3.24/alpha/cru_alpha_1901.mat'))
cru_alpha_1901_colnames = colnames(read.csv('~/Documents/Data/CRU3.24/alpha/cru_alpha_1901.csv'))
colnames(cru_alpha_1901) = cru_alpha_1901_colnames
head(cru_alpha_1901)
subset(cru_alpha_1901, Lat == 38.75 & Lon == -106.75) # 32590
subset(cru_alpha_1901, Lat == 38.75 & Lon == -107.25) # 32589
row_almont_cbt_road = 32590
row_pbm_pfeiler = 32589


cru_almont_cbt_road=c()
for (yeari in 1940:2015) {
  
    month = c(1:12)
    year = rep(yeari, 12)
    tmp=as.vector(readMat(paste('~/Documents/Data/CRU3.24/tmp2/cru_tmp_',yeari,'.mat',sep='')))[[1]][32590,3:14]
    par=as.vector(readMat(paste('~/Documents/Data/CRU3.24/par/cru_par_',yeari,'.mat',sep='')))[[1]][32590,3:14]
    vpd=as.vector(readMat(paste('~/Documents/Data/CRU3.24/vpd2/cru_vpd_',yeari,'.mat',sep='')))[[1]][32590,3:14]
    pre=as.vector(readMat(paste('~/Documents/Data/CRU3.24/pre2/cru_pre_',yeari,'.mat',sep='')))[[1]][32590,3:14]
    
    per_year = cbind(month, year, tmp, par, vpd, pre)
    cru_almont_cbt_road = rbind(cru_almont_cbt_road, per_year) 
    
  }

cru_pbm_pfeiler=c()
for (yeari in 1940:2015) {
  
  month = c(1:12)
  year = rep(yeari, 12)
  tmp=as.vector(readMat(paste('~/Documents/Data/CRU3.24/tmp2/cru_tmp_',yeari,'.mat',sep='')))[[1]][32589,3:14]
  par=as.vector(readMat(paste('~/Documents/Data/CRU3.24/par/cru_par_',yeari,'.mat',sep='')))[[1]][32589,3:14]
  vpd=as.vector(readMat(paste('~/Documents/Data/CRU3.24/vpd2/cru_vpd_',yeari,'.mat',sep='')))[[1]][32589,3:14]
  pre=as.vector(readMat(paste('~/Documents/Data/CRU3.24/pre2/cru_pre_',yeari,'.mat',sep='')))[[1]][32589,3:14]
  
  per_year = cbind(month, year, tmp, par, vpd, pre)
  cru_pbm_pfeiler = rbind(cru_pbm_pfeiler, per_year) 
  
}

cru_almont = as.data.frame(cru_almont_cbt_road)
cru_almont$site = 'almont'
cru_cbt = as.data.frame(cru_almont_cbt_road)
cru_cbt$site = 'cbt'
cru_road = as.data.frame(cru_almont_cbt_road)
cru_road$site = 'road'
cru_pbm = as.data.frame(cru_pbm_pfeiler)
cru_pbm$site = 'pbm'
cru_pfeiler = as.data.frame(cru_pbm_pfeiler)
cru_pfeiler$site = 'pfeiler'

cru_all = rbind(cru_almont, cru_cbt, cru_road, cru_pbm, cru_pfeiler)
head(cru_all)

## add in lat lon
rmbl_data_merge <- left_join(rmbl_data_raw, rmbl_latlon, by = 'site')
head(rmbl_data_merge)
rmbl_data_merge$year = as.numeric(rmbl_data_merge$year)
rmbl_data_merge$month = as.numeric(rmbl_data_merge$month)

## add in climate
rmbl_data_clim_merge <- left_join(rmbl_data_merge, cru_all, by = c('site', 'year', 'month'))
head(rmbl_data_clim_merge)
tail(rmbl_data_clim_merge)

## write it out
write.csv(rmbl_data_clim_merge, '../data/rmbl_data_climate_merge.csv')

