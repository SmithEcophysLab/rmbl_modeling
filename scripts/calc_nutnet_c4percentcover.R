# Find C4 percent cover at NutNet sites

library(dplyr)

# load data files
all_nutnet_cover <- read.csv("../data/full-cover-2022-01-31.csv")
all_sites <- read.csv("../data/pmodel_site_sims_coords.csv")

# remove non-living groups and non-control plots
control_cover_living <- subset(all_nutnet_cover, functional_group != "NON-LIVE" & functional_group != "NULL") %>%
  subset(trt == "Control")
unique(control_cover_living$functional_group)

# calculate total C4 cover per year per site per plot
C4_cover <- control_cover_living %>% 
  group_by(year, site_code, plot) %>% 
  summarise("c4_percent" = (sum(max_cover[ps_path == "C4"]) * 100 / sum(max_cover)))

# subset NutNet cover data to most recent year
C4_cover_recent <- C4_cover %>% 
  group_by(site_code, plot) %>%
  filter(row_number() == which.max(year)) %>%
  select(everything())

# average across plots
C4_cover_avg <- C4_cover_recent %>%
  group_by(site_code) %>%
  summarise("c4_percent" = mean(c4_percent))

# add NutNet C4 cover to LEMONTREE simulation dataset
names(C4_cover_avg) <- c("site", "c4_percent")
all_sites_C4 <- full_join(all_sites, C4_cover_avg, by = "site")

# set NutNet sites with 0% C4 cover to 100% C3 cover
all_sites_C4$c3_percent[all_sites_C4$experiment == "nutnet"] <- 100 - all_sites_C4$c4_percent[all_sites_C4$experiment == "nutnet"]

write.csv(all_sites_C4, "../data/pmodel_site_sims_coords_c4.csv")

