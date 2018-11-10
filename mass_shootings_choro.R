
library(tidyverse)
library(viridis)

# pulling and manipulating mass shooting data

mass_shooting <- 
  read_csv("https://raw.githubusercontent.com/StanfordGeospatialCenter/MSA/master/Data/Stanford_MSA_Database.csv") %>% 
  mutate(count = 1) %>% 
  rename(year = Date, 
         city = City, 
         state = State, 
         num_fatal = "Total Number of Fatalities", 
         num_victim = "Total Number of Victims") %>% 
  select(year, city, state, count, num_fatal, num_victim) %>% 
  mutate(year = str_pad(year, width = 10, side = "left", "0"), 
         year = substring(year , 7, 10), 
         year = as_factor(year), 
         state = str_to_lower(state))

# getting US geo data

states <- map_data("state") %>% 
  rename(state = region)

# joing US and shooting data

mass_counts <- mass_shooting %>% 
  group_by(state) %>% 
  summarise(n = n())

mass_victims <- mass_shooting %>% 
  group_by(state) %>% 
  summarize(total_vic = sum(num_victim))

mass_counts_location <- left_join(states, mass_counts, by = "state") %>% 
  mutate(n = ifelse(is.na(n), 0, n))

mass_vic_location <- left_join(states, mass_victims, by = "state") %>% 
  mutate(total_vic = ifelse(is.na(total_vic), 0, total_vic))

# Aggregate maps

mass_counts_location %>% 
  ggplot() + 
  geom_polygon(aes(x = long, y = lat, fill = n, group = group), color = NA, alpha = 0.9) + 
  labs(title = "Number of mass shootings in the 48 continguous states, 1966 - 2016", 
       caption = "Data source: Stanford Mass Shootings in America, courtesy of the Stanford Geospatial Center and Stanford Libraries") +
  scale_fill_viridis(breaks = c(0, 5, 10, 15, 20, 25, 30, 35, 40), 
                     name = "Number of mass shootings", 
                     guide = guide_colorbar(barheight = unit(3, units = "mm"),
                                            barwidth = unit(70, units = "mm"),
                                            title.position = 'top', nrow = 1, 
                     ticks = FALSE)) +
  theme_void() + 
  coord_map() + 
  theme(plot.background = element_rect(fill = "#f5f5f2", color = NA), 
        legend.position = c(0.15, 0.09), 
        legend.direction = "horizontal", 
        plot.caption = element_text(size = 8, color = "#4e4d47",
                                    margin = margin(b = 0.3, unit = "cm"), hjust = 0.98), 
        legend.title = element_text(size = 10), 
        plot.title = element_text(size = 13, hjust = 0.01, color = "#4e4d47", 
                                  margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")))

mass_vic_location %>% 
  ggplot() + 
  geom_polygon(aes(x = long, y = lat, fill = total_vic, group = group), color = NA, alpha = 0.9) + 
  labs(title = "Number of victims due to mass shootings in the 48 continguous states, 1966 - 2016", 
       caption = "Data source: Stanford Mass Shootings in America, courtesy of the Stanford Geospatial Center and Stanford Libraries") +
  scale_fill_viridis(name = "Number of victims", 
                     guide = guide_colorbar(barheight = unit(3, units = "mm"),
                                            barwidth = unit(70, units = "mm"),
                                            title.position = 'top', nrow = 1, 
                                            ticks = FALSE)) +
  theme_void() + 
  coord_map() + 
  theme(plot.background = element_rect(fill = "#f5f5f2", color = NA), 
        legend.position = c(0.15, 0.09), 
        legend.direction = "horizontal", 
        plot.caption = element_text(size = 8, color = "#4e4d47",
                                    margin = margin(b = 0.3, unit = "cm"), hjust = 0.98), 
        legend.title = element_text(size = 10, color = "#4e4d47"), 
        plot.title = element_text(size = 13, hjust = 0.01, color = "#4e4d47", 
                                  margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")))

