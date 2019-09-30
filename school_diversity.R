rm(list = ls())

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
         original_diverse = lag(diverse, default = first(diverse)),
         original_diverse_pct = lag(round(100 - white,2), default = first(white)),
         pct_change = 
           round((diverse_pct - original_diverse_pct)/original_diverse_pct,2)) %>%
  select(geoid,lea_name,st,d_locale_txt,school_year,diverse:pct_change)  %>%
  filter(school_year == "2016-2017") %>%
  mutate(change_type = 
           case_when(
             original_diverse == "Extremely undiverse" & diverse == "Diverse" ~ "Somewhat Increased Diversity", 
             original_diverse == "Extremely undiverse" & diverse == "Undiverse" ~ "Somewhat Increased Diversity",
             original_diverse == "Extremely undiverse" & diverse == "Extremely undiverse" ~ "Still Undiverse", #thought about using `Still Extremely undiverse` but decided against because the point of the heat map (is that what this is called?) is to highlight the change, not to penalize districts with existing lack of diversity
             original_diverse == "Diverse" & diverse == "Diverse" ~ "Still Diverse",
             original_diverse == "Diverse" & diverse == "Undiverse" ~ "Somewhat Decreased Diversity",
             original_diverse == "Diverse" & diverse == "Extremely undiverse" ~ "Significantly Decreased Diversity",
             original_diverse == "Undiverse" & diverse == "Undiverse" ~ "Still Undiverse",
             original_diverse == "Undiverse" & diverse == "Diverse" ~ "Somewhat Increased Diversity",
             original_diverse == "Undiverse" & diverse == "Extremely undiverse" ~ "Somewhat Decreased Diversity",
             TRUE ~ "Other"
           )) %>%
  ungroup()

# looks like I was wrong about the lag function. It returns the first value for any values where
# there are two observations. Looking at the code chunk as a whole, the lag functions set up the 
# case_when function so we can get a look at the change from '95 to '17 since we see the first measure
# classification then the second measure classification, then filter for just 16/17. I'm going to change
# the variable to original_diverse/pct to clear up some confusion on my end.

mo_schools_diverse_chg$change_type <- factor(mo_schools_diverse_chg$change_type,
                                             levels = c("Significantly Increased Diversity",
                                                        "Somewhat Increased Diversity",
                                                        "Still Diverse",
                                                        "Still Undiverse",
                                                        "Somewhat Decreased Diversity",
                                                        "Significantly Decreased Diversity",
                                                        "Other"
                                                        ))


# following the code from acircleda, they create this new summary of the number of observations in each
# category. Doesn't look like the case_when mutate function is needed here. Could just do a count of 
# the diverse variable.

diversity_pct <- 
  mo_schools_diverse_chg %>%
  mutate(diversity_factor = 
    case_when(
      diverse == "Diverse" ~ "Diverse",
      diverse == "Undiverse" ~ "Undiverse",
      diverse == "Extremely undiverse" ~ "Undiverse"
  )) %>%
  count(diverse)

# I think I could have joined the shapefile with the mo_schools_diverse_chg data in the code above as
# just another pipe but I'm not too familiar with using shapefiles or building plots of maps so I'm just
# going to go with what acircleda did and see what happens.

diverse_w_shape <- 
  mo_schools_diverse_chg %>%
  left_join(mosf, by = c("geoid" = "GEOID"))   #I think leaid was changed for this join but wasn't necessary. After finishing I'll clean that part up

# not sure what variables are needed for plot but diverse_w_shape has a lot in it...might clean up
# create the visual first and go back later

#write.csv(diverse_w_shape, "~/R/Git/Project/TidyTuesday/diverse_w_shape.csv")

mo_map <- ggplot(diverse_w_shape) +
  geom_sf(data = diverse_w_shape, size = 0.2, color = "#616161", aes(fill = change_type, geometry = geometry)) +
  coord_sf() +
  scale_fill_manual(values = c("#C49AC7", "#EFA8C8", "#F4EA8F", "#B6E284", "#5DB9CF")) +
  labs(
    fill = "Change Categories",
    title = "Change in Student Diversity from 1994 to 2017",
    subtitle = "For Missouri School Districts",
    caption = "#TidyTuesday visual by Jim Felps | Data from NCES"
  ) +
  theme(
    panel.background = element_rect(fill = "white"),
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    plot.title = element_text(color = "#023858", size = 16, face = "bold"),
    plot.subtitle = element_text(color = "#023858"),
    legend.title = element_text(face = "bold")
  )
  
# pretty sure I got the categories backwards based on the plot. Looks like all rural districts in MO
# had a significant increase in diversity, which I'm positive isn't the case from both personal knowledge
# and some of the other visualizations from this TT

ggsave("~/R/Git/Project/TidyTuesday/mo_map.png", mo_map, width = 16, height = 9, dpi = 320)

