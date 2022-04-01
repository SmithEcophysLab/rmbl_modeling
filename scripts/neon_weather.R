
library(dplyr)
library(tidyr)
library(data.table)

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

daily_humid <- import_csvs(filepath = "../data/NEON_weather/daily_humid") %>%
  separate(col = "V1", into = c("experiment", "site_id", "site"), sep = "[[:punct:]]")

daily_precip <- import_csvs(filepath = "../data/NEON_weather/daily_precip") %>%
  separate(col = "V1", into = c("experiment", "site_id", "site"), sep = "[[:punct:]]")

daily_pres <- import_csvs(filepath = "../data/NEON_weather/daily_pres") %>%
  separate(col = "V1", into = c("experiment", "site_id", "site"), sep = "[[:punct:]]")

daily_shortRad <- import_csvs(filepath = "../data/NEON_weather/daily_shortRad") %>%
  separate(col = "V1", into = c("experiment", "site_id", "site"), sep = "[[:punct:]]")

daily_temp <- import_csvs(filepath = "../data/NEON_weather/daily_temp") %>%
  separate(col = "V1", into = c("experiment", "site_id", "site"), sep = "[[:punct:]]")

neon_weather <- full_join(daily_humid, daily_precip, by = c("date", "experiment", "site_id", "site")) %>%
  full_join(daily_pres, by = c("date", "experiment", "site_id", "site")) %>%
  full_join(daily_shortRad, by = c("date", "experiment", "site_id", "site")) %>%
  full_join(daily_temp, by = c("date", "experiment", "site_id", "site"))

daily_neon_weather <- neon_weather %>% 
  separate(col = "date", into = c("year", "month", "day"), sep = "[[:punct:]]") %>% 
  select("experiment", "site_id", "site", everything())

write.csv(daily_neon_weather, "output/daily_neon_weather.csv")
