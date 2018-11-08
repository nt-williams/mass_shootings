
library(tidyverse)

# pulling data

mass_shooting <- 
  read_csv("https://raw.githubusercontent.com/StanfordGeospatialCenter/MSA/master/Data/Stanford_MSA_Database.csv")

# getting US geo data

states <- map_data("state")

s_map <- states %>% 
  ggplot() + 
  geom_polygon(aes(x = long, y = lat, group = group), color = "white") + 
  theme_void()
