library(tidyverse)
library(janitor)

four_wheel_drive <- c("4-Wheel or All-Wheel Drive", "All-Wheel Drive", "4-Wheel Drive")

big_epa_cars <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-15/big_epa_cars.csv")

cars_clean <- big_epa_cars %>%
  clean_names()

# what makes are there 
View(cars_clean %>%
  count(make) %>%
  arrange(desc(n)))

# a lot of normal makes, I can see why others have chosen to filter for just a few brands that make up a significant portion of the data

View(cars_clean %>%
       count(fuel_type) %>%
       arrange(desc(n)))

# majority are gasoline engines (no shock) with quite a few cars requiring premium gasoline
# might explore something around diesel cars. not sure yet

View(cars_clean %>%
       count(eng_dscr) %>%
       arrange(desc(n)))
# this will look at engine/motor descriptors used by the EPA (https://www.fueleconomy.gov/feg/findacarhelp.shtml#engine)
# lots of feedback fuel and spark ignition direct injection, also not a shock.
# there are some guzzlers in the data. might be interesting to look at what models they are

# engine description is strange because a lot of these vehicles fall under several descriptors but it looks like they're only classifying a car as two or three max

View(cars_clean %>%
       filter(drive == "4-Wheel Drive") %>%
       count(model))
  
# STEPS
# clean up drive categories -- create "truck 2WD" and "truck 4WD" and combine AWD into one