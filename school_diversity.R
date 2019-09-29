library(tidyverse)
library(sf)
library(janitor)

# school data from r4ds github
school_diversity <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-24/school_diversity.csv") %>%
  clean_names(case = "snake")

# read in shapefile from US Census Bureau (https://www.census.gov/cgi-bin/geo/shapefiles/index.php)
mosf <- read_sf("~/R/Git/Project/TidyTuesday/tl_2019_29_unsd/tl_2019_29_unsd.shp")

#filter for only MO schools
mo_schools <- 
  school_diversity %>%
  filter(st == "MO") %>%
  rename(geoid = leaid)

# make diversity a factor
mo_schools$diverse <- factor(mo_schools$diverse)


# reading about lag and I'm slightly confused but I based on what I see in the dataset, there
# are two observations for every school district, one for the 94-95 measurement & another 
# for the 16-17. If the district didn't exist for both then there is only one observation. This 
# leads me to believe that the lag function will grab both observations for a given school ID

mo_schools_diverse_chg <- 
  mo_schools %>%
  group_by(geoid) %>%
  mutate(diverse_pct = round(100 - white,2),
         org_diverse = lag(diverse, default = first(diverse)),
         org_diverse_pct = lag(round(100 - white,2), default = first(white)),
         pct_change = 
           round((diverse_pct - org_diverse_pct)/org_diverse_pct,2)) %>%
  select(geoid,lea_name,st,d_locale_txt,school_year,diverse:pct_change)

# looks like I was wrong about the lag function. It returns the first value for any values where
# there are two observations. I'm a bit confused about the pct_change on the first measurement so 
# I'll have to think that one out when I'm less tired.
