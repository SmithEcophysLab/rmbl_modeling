# Find C4 percent cover at NutNet sites

library(dplyr)
library(tidyr)
library(purrr)

## load LEMONTREE simulation dataset
all_sites <- read.csv("../data/pmodel_site_sims_coords.csv")

## NutNet
# load nutnet data
all_nutnet_cover <- read.csv("../data/full-cover-2022-01-31.csv")

# remove non-living groups and non-control plots
nutnet_control_living <- subset(all_nutnet_cover, functional_group != "NON-LIVE" & functional_group != "NULL") %>%
  subset(trt == "Control")
unique(nutnet_control_living$functional_group)

# calculate total C4 cover per year per site per plot
C4_nutnet_cover <- nutnet_control_living %>% 
  group_by(year, site_code, plot) %>% 
  summarise("c4_percent" = (sum(max_cover[ps_path == "C4"]) * 100 / sum(max_cover)))

# subset NutNet cover data to most recent year
C4_nutnet_recent <- C4_nutnet_cover %>% 
  group_by(site_code, plot) %>%
  filter(row_number() == which.max(year)) %>%
  select(everything())

# average across plots
C4_nutnet_avg <- C4_nutnet_recent %>%
  group_by(site_code) %>%
  summarise("c4_percent" = mean(c4_percent))

## NEON
# load data
# National Ecological Observatory Network. 2022. Data Product DP1.10058.001, Plant presence and percent cover. Provisional data downloaded from https://data.neonscience.org on February 23, 2022. Battelle, Boulder, CO, USA NEON. 2022.
all_neon_cover <-  list.files(path = "../data/NEON_presence-cover-plant/NEON_recent",
                      pattern = "*.csv", 
                      full.names = T) %>% 
  map_df(~read.csv(., header = TRUE))
unique(all_neon_cover$siteID)

all_neon_cover_living <- subset(all_neon_cover, all_neon_cover$divDataType == "plantSpecies")
unique(all_neon_cover_living$siteID)

C4_lineages <- read.csv("../data/C4 lineages.csv")

# assign photosynthetic pathway
neon_cover <- separate(all_neon_cover_living, scientificName, c("genus", "species"), "[[:space:]]", remove = FALSE)
neon_C4_fam <- neon_cover[neon_cover$family %in% C4_lineages$Family, ]
neon_C4_gen <- neon_C4_fam[neon_C4_fam$genus %in% C4_lineages$Genus, ]

unique(neon_C4_gen$genus)

C4_lineages_neon <- C4_lineages[C4_lineages$Genus %in% unique(neon_C4_gen$genus), ]

neon_cover$ps_path[neon_cover$genus %in% C4_lineages_neon$Genus[C4_lineages_neon$Only_C4 == "Yes"]] <- "C4"

neon_cover$ps_path[neon_cover$genus == "Aristida"] <- "C4"
neon_cover$ps_path[neon_cover$genus == "Atriplex"] <- "C4"
neon_cover$ps_path[neon_cover$genus == "Mollugo"] <- "C4"
neon_cover$ps_path[neon_cover$genus == "Cyperus" & 
                      neon_cover$species == "croceus"] <- "C4"
neon_cover$ps_path[neon_cover$genus == "Cyperus" & 
                      neon_cover$species == "plukenetii"] <- "C4"
neon_cover$ps_path[neon_cover$genus == "Cyperus" & 
                      neon_cover$species == "retrorsus"] <- "C4"
neon_cover$ps_path[neon_cover$genus == "Cyperus" & 
                      neon_cover$species == "sp."] <- "C4"
neon_cover$ps_path[neon_cover$genus == "Panicum" & 
                      neon_cover$species == "capillare"] <- "C4"
neon_cover$ps_path[neon_cover$genus == "Panicum" & 
                      neon_cover$species == "hirticaule"] <- "C4"
neon_cover$ps_path[neon_cover$genus == "Panicum" & 
                      neon_cover$species == "virgatum"] <- "C4"

neon_cover$ps_path[is.na(neon_cover$ps_path)] <- "C3"

# calculate total C4 cover per year per site per plot
C4_neon_cover <- neon_cover %>% 
  group_by(siteID, plotID) %>% 
  summarise("c4_percent" = (sum(percentCover[ps_path == "C4"]) * 100 / sum(percentCover)))

# average across plots
C4_neon_avg <- C4_neon_cover %>%
  group_by(siteID) %>%
  summarise("c4_percent" = mean(c4_percent))

# add NEON C4 cover to LEMONTREE simulation dataset
names(C4_neon_avg) <- c("site", "c4_percent")
names(C4_nutnet_avg) <- c("site", "c4_percent")
nutnet_neon <- bind_rows(C4_nutnet_avg, C4_neon_avg)
all_sites_nutnet_neon <- full_join(all_sites, nutnet_neon, by = "site")

# set sites with 0% C4 cover to 100% C3 cover
all_sites_nutnet_neon$c3_percent <- 100 - all_sites_nutnet_neon$c4_percent

write.csv(all_sites_nutnet_neon, "../data/pmodel_site_sims_coords_c4.csv")

