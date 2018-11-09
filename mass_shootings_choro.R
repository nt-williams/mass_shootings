
library(tidyverse)
library(lubridate)

# pulling and manipulating mass shooting data

mass_shooting <- 
  read_csv("https://raw.githubusercontent.com/StanfordGeospatialCenter/MSA/master/Data/Stanford_MSA_Database.csv") %>% 
  mutate(count = 1) %>% 
  rename(year = Date, 
         city = City, 
         state = State) %>% 
  select(year, city, state, count) %>% 
  mutate(year = str_pad(year, width = 10, side = "left", "0"), 
         year = substring(year , 7, 10), 
         year = as_factor(year))

# getting US geo data

states <- map_data("state")

# joing US and shooting data



# blank US map

s_map <- states %>% 
  ggplot() + 
  geom_polygon(aes(x = long, y = lat, group = group), color = "white") + 
  theme_void()

