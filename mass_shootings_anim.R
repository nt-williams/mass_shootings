
library(tidyverse)
library(viridis)
library(magick)

# importing and cleaning shooting data

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

# US geo data

states <- map_data("state") %>% 
  rename(state = region)

# summarizing across states for total num victims in each year

mass_victims <- mass_shooting %>% 
  group_by(state, year) %>% 
  summarise(n = sum(num_victim))

mass_victims <- split(mass_victims, mass_victims$year)

# joing geo data with shooting data and dealing with states that have 0 victims

mass_location <- list()

for (i in names(mass_victims)) {
  mass_location[[i]] <- left_join(states, mass_victims[[i]], by = "state") %>% 
    mutate(n = ifelse(is.na(n), 0, n), 
           year = i, 
           long = as.double(long), 
           lat = as.double(lat)) %>% 
    select(-subregion)
}

mass_location <- bind_rows(mass_location)

# Making gif

shooting_map <- function(yr) {
  
  mass_location %>% 
    filter(year == yr) %>% 
    ggplot() + 
    geom_polygon(aes(x = long, y = lat, fill = n, group = group), color = NA, alpha = 0.8) + 
    labs(title = "Number of mass shootings in the 48 continguous states", 
         caption = "Source: Stanford Mass Shootings in America, courtesy of the Stanford Geospatial Center and Stanford Libraries", 
         subtitle = paste("Year:", yr, sep = " ")) +
    scale_fill_viridis(name = "Number of victims", 
                       guide = guide_colorbar(barheight = unit(2, units = "mm"),
                                              barwidth = unit(30, units = "mm"),
                                              title.position = 'top', nrow = 1, 
                                              ticks = FALSE),
                       limits = c(0, 160)) + 
    theme_void() + 
    coord_map() + 
    theme(plot.background = element_rect(fill = "#f5f5f2", color = NA), 
          legend.position = c(0.15, 0.09), 
          legend.direction = "horizontal", 
          plot.caption = element_text(size = 6, color = "#4e4d47",
                                      margin = margin(b = 0.3, unit = "cm"), hjust = 0.98), 
          legend.title = element_text(size = 8, color = "#4e4d47"), 
          legend.text = element_text(size = 7, color = "#4e4d47"), 
          plot.title = element_text(size = 10, hjust = 0.01, color = "#4e4d47", 
                                    margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")), 
          plot.subtitle = element_text(size = 8, hjust = 0.006, color = "#4e4d47", 
                                       margin = margin(b = -0.1, t = 0.25, l = 10, unit = "cm"))) -> mp
  
  file_out <- sprintf("mp-ms-%s.png", yr)
  ggsave(file_out, mp, width = 8, height = 5)
  
  file_out
}

year <- levels(mass_shooting$year)

year %>% 
  purrr::map(shooting_map) %>% 
  purrr::map(image_read) %>% 
  image_join() %>% 
  image_animate(fps = 2) %>% 
  image_write("shooting.gif")
  
  

