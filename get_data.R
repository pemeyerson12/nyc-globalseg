library(tidyverse)
library(tidycensus)
library(sf)
library(mapview)
library(tibble)
library(iscoCrosswalks)
library(tigris)
library(stringr)

fuaNY <- c("Bronx", "Kings", "Nassau", "New York", "Queens", "Richmond", "Rockland", "Suffolk", "Westchester")
fuaNJ <- c("Bergen", "Essex", "Hudson", "Middlesex", "Monmouth", "Ocean", "Passaic", "Union")

# get 2000 census occupation data
vars00 <- load_variables(2000, "sf3") %>%
  filter(str_detect(concept, "OCCUPATION"))

soc00_NY <- get_decennial(
  geography = "tract",
  variables = vars00$name,
  state     = "NY",
  year      = 2000
)

soc00_NJ <- get_decennial(
  geography = "tract",
  variables = vars00$name,
  state     = "NJ",
  year      = 2000
)

# get 2007-2011 ACS occupation data
vars11 <- load_variables(2011, "acs5") %>%
  filter(str_detect(name, "^C24010") & concept == "SEX BY OCCUPATION FOR THE CIVILIAN EMPLOYED POPULATION 16 YEARS AND OVER")

soc11_NY <- get_acs(
  geography = "tract",
  variables = vars11$name,
  state     = "NY",
  year      = 2011
)

soc11_NJ <- get_acs(
  geography = "tract",
  variables = vars11$name,
  state     = "NJ",
  year      = 2011
)

# get 2017-2021 ACS occupation data
vars21 <- load_variables(2021, "acs5") %>%
  filter(str_detect(name, "^C24010") & concept == "SEX BY OCCUPATION FOR THE CIVILIAN EMPLOYED POPULATION 16 YEARS AND OVER")

soc21_NY <- get_acs(
  geography = "block group",
  variables = vars21$name,
  state     = "NY",
  year      = 2021
)

soc21_NJ <- get_acs(
  geography = "block group",
  variables = vars21$name,
  state     = "NJ",
  year      = 2021
)

# write.csv(vars00, "vars00.csv")
# write.csv(vars11, "vars11.csv")
# write.csv(vars21, "vars21.csv")

xwalk00 <- read.csv("vars00.csv") %>% 
  filter(category != "z" & category != "AGR") %>%
  dplyr::rename(variable = name)

xwalk11 <- read.csv("vars11.csv") %>% 
  filter(category != "z") %>%
  dplyr::rename(variable = name)

xwalk21 <- read.csv("vars21.csv") %>% 
  filter(category != "z") %>%
  dplyr::rename(variable = name)

x00_NJ <- soc00_NJ %>% left_join(xwalk00) %>% 
  filter(!is.na(category)) %>%
  group_by(GEOID, group, category) %>%
  summarize(jobs = sum(value)) %>%
  arrange(GEOID, group, category)

x00_NY <- soc00_NY %>% left_join(xwalk00) %>% 
  filter(!is.na(category)) %>%
  group_by(GEOID, group, category) %>%
  summarize(jobs = sum(value)) %>%
  arrange(GEOID, group, category)

t11_NJ <- soc11_NJ %>% left_join(xwalk11) %>% 
  filter(!is.na(category)) %>%
  group_by(GEOID, group, category) %>%
  summarize(jobs = sum(estimate)) %>%
  arrange(GEOID, group, category)

t11_NY <- soc11_NY %>% left_join(xwalk11) %>% 
  filter(!is.na(category)) %>%
  group_by(GEOID, group, category) %>%
  summarize(jobs = sum(estimate)) %>%
  arrange(GEOID, group, category)

x21_NJ <- soc21_NJ %>% left_join(xwalk21) %>% 
  filter(!is.na(category)) %>%
  group_by(GEOID, group, category) %>%
  summarize(jobs = sum(estimate)) %>%
  arrange(GEOID, group, category)

x21_NY <- soc21_NY %>% left_join(xwalk21) %>% 
  filter(!is.na(category)) %>%
  group_by(GEOID, group, category) %>%
  summarize(jobs = sum(estimate)) %>%
  arrange(GEOID, group, category)

# 2000 census tracts to 2010 census tracts (t = translated, in dataframe name)
nj0010 <- read.csv("nj_00_10tract.csv") %>% 
  dplyr::select(tr2000ge, tr2010ge, wt_adult)
ny0011 <- read.csv("ny_00_11tract.csv") %>% 
  dplyr::select(tr2000ge, tr2011ge, wt_adult)

t00_NJ <- x00_NJ %>% dplyr::mutate(tr2000ge = as.double(GEOID)) %>% 
  left_join(nj0010, relationship = "many-to-many") %>%
  dplyr::mutate(wt_jobs = jobs * wt_adult) %>% 
  group_by(category, tr2010ge) %>% summarize(jobs = round(sum(wt_jobs), 0)) %>%
  dplyr::rename(GEOID = tr2010ge)

t00_NY <- x00_NY %>% dplyr::mutate(tr2000ge = as.double(GEOID)) %>% 
  left_join(ny0011, relationship = "many-to-many") %>%
  dplyr::mutate(wt_jobs = jobs * wt_adult) %>% 
  group_by(category, tr2011ge) %>% summarize(jobs = round(sum(wt_jobs), 0)) %>%
  dplyr::rename(GEOID = tr2011ge)

# 2021 block groups to 2010 census tracts
nj2010 <- read.csv("nj_20bg_10tract.csv") %>% 
  dplyr::select(bg2020ge, tr2010ge, wt_adult)

ny2011 <- read.csv("ny_20bg_11tract.csv") %>% 
  dplyr::select(bg2020ge, tr2011ge, wt_adult)

t21_NJ <- x21_NJ %>% dplyr::mutate(bg2020ge = as.double(GEOID)) %>% 
  left_join(nj2010, relationship = "many-to-many") %>%
  dplyr::mutate(wt_jobs = jobs * wt_adult) %>% 
  group_by(category, tr2010ge) %>% summarize(jobs = round(sum(wt_jobs), 0)) %>%
  dplyr::rename(GEOID = tr2010ge)

t21_NY <- x21_NY %>% dplyr::mutate(bg2020ge = as.double(GEOID)) %>% 
  left_join(ny2011, relationship = "many-to-many") %>%
  dplyr::mutate(wt_jobs = jobs * wt_adult) %>% 
  group_by(category, tr2011ge) %>% summarize(jobs = round(sum(wt_jobs), 0)) %>%
  dplyr::rename(GEOID = tr2011ge)

# filter to counties
countiesNJ <- tigris::counties(state = "NJ", year = 2010) %>%
  filter(NAME10 %in% fuaNJ) %>%
  dplyr::select(GEOID10, NAME10, geometry)

countiesNY <- tigris::counties(state = "NY", year = 2011) %>%
  filter(NAME %in% fuaNY) %>%
  dplyr::select(GEOID, NAME, geometry)

regexNJ <- str_c(countiesNJ$GEOID10, collapse = "|")
regexNY <- str_c(countiesNY$GEOID, collapse = "|")

fua00_NJ <- t00_NJ %>% ungroup() %>% filter(str_detect(as.character(GEOID), regexNJ))
fua00_NY <- t00_NY %>% ungroup() %>% filter(str_detect(as.character(GEOID), regexNY))

fua11_NJ <- t11_NJ %>% ungroup() %>% filter(str_detect(as.character(GEOID), regexNJ))
fua11_NY <- t11_NY %>% ungroup() %>% filter(str_detect(as.character(GEOID), regexNY))

fua21_NJ <- t21_NJ %>% ungroup() %>% filter(str_detect(as.character(GEOID), regexNJ))
fua21_NY <- t21_NY %>% ungroup() %>% filter(str_detect(as.character(GEOID), regexNY))

write.csv(bind_rows(fua00_NJ, fua00_NY), "fua00.csv")
write.csv(bind_rows(fua11_NJ, fua11_NY), "fua11.csv")
write.csv(bind_rows(fua21_NJ, fua21_NY), "fua21.csv")