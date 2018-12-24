
library(tidyverse)
library(viridis)
library(magick)
library(rvest)
library(googlesheets)

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

# shooting tracker data 2017

s_tracker_2017 <- read_csv("./data/shooting_tracker_2017.csv") %>% 
  janitor::clean_names() %>% 
  separate(incident_date, into = c("day", "year"), sep = ", ") %>% 
  mutate(num_victim = number_killed + number_injured, 
         count = 1, 
         state = str_to_lower(state), 
         year = as_factor(year)) %>% 
  rename(num_fatal = number_killed, 
         city = city_or_county) %>% 
  select(year, city, state, count, num_fatal, num_victim)

# shooting tracker data 2018, begins '18 isn't completed, pulling data directly from URl for ease of updating

base <- "https://www.gunviolencearchive.org/reports/mass-shooting?page="

url_pages <- str_c(base, 0:12)

s_tracker_2018 <- list()

for (i in 1:length(url_pages)) {
  s_tracker_2018[[i]] <- read_html(url_pages[i]) %>% 
    html_nodes(css = "table") %>% 
    .[[1]] %>% 
    html_table() %>% 
    as_tibble() %>% 
    janitor::clean_names() %>% 
    separate(incident_date, into = c("day", "year"), sep = ", ") %>% 
    mutate(num_victim = number_killed + number_injured, 
           count = 1, 
           state = str_to_lower(state), 
           year = as_factor(year)) %>% 
    rename(num_fatal = number_killed, 
           city = city_or_county) %>% 
    select(year, city, state, count, num_fatal, num_victim)
}

s_tracker_2018 <- bind_rows(s_tracker_2018)

# adding shooting tracker data to stanford data

mass_shooting <- mass_shooting %>% 
  bind_rows(s_tracker_2017) %>% 
  bind_rows(s_tracker_2018) %>% 
  mutate(year = as_factor(year),
         year = fct_relevel(year, "1966", "1971"), 
         year = fct_relevel(year, "2017", "2018", after = Inf)) 

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
    geom_polygon(aes(x = long, y = lat, fill = n, group = group), color = "white", alpha = 0.8, size = 0.1) + 
    labs(title = "Number of victims due to mass shootings in the 48 continguous states", 
         caption = "Source: Stanford Mass Shootings in America, courtesy of the Stanford Geospatial Center and Stanford Libraries", 
         subtitle = paste("Year:", yr, sep = " ")) +
    scale_fill_viridis(name = "Number of victims", 
                       guide = guide_colorbar(barheight = unit(2, units = "mm"),
                                              barwidth = unit(30, units = "mm"),
                                              title.position = 'top', nrow = 1, 
                                              ticks = FALSE),
                       limits = c(0, 160)) + 
    theme_void() + 
    coord_map(projection = "albers", lat0 = 30, lat1 = 40) + 
    theme(plot.background = element_rect(fill = "#f0f0f0", color = NA), 
          legend.position = c(0.15, 0.09), 
          legend.direction = "horizontal", 
          plot.caption = element_text(size = 6, color = "#4e4d47",
                                      margin = margin(b = 0.3, unit = "cm"), hjust = 0.98), 
          legend.title = element_text(size = 8, color = "#4e4d47"), 
          legend.text = element_text(size = 7, color = "#4e4d47"), 
          plot.title = element_text(size = 10, hjust = 0.075, color = "#4e4d47", 
                                    margin = margin(t = 0.5, b = -0.1, l = 2, unit = "cm")), 
          plot.subtitle = element_text(size = 8, hjust = 0.037, color = "#4e4d47", 
                                       margin = margin(b = -0.1, t = 0.25, l = 2, unit = "cm"))) -> mp
  
  file_out <- sprintf("mp-ms-%s.png", yr)
  ggsave(file_out, mp, width = 8, height = 5)
  
  file_out
}

year <- sort(levels(mass_shooting$year))

year %>% 
  purrr::map(shooting_map) %>% 
  purrr::map(image_read) %>% 
  image_join() %>% 
  image_animate(fps = 2) %>% 
  image_write("shooting.gif")
  
# gif with new years

# shooting_map <- function(yr) {
#   
#   mass_location %>% 
#     filter(year == yr) %>% 
#     ggplot() + 
#     geom_polygon(aes(x = long, y = lat, fill = n, group = group), color = NA, alpha = 0.8) + 
#     labs(title = "Number of victims due to mass shootings in the 48 continguous states", 
#          caption = "Source: Stanford Mass Shootings in America, courtesy of the Stanford Geospatial Center and Stanford Libraries", 
#          subtitle = paste("Year:", yr, sep = " ")) +
#     scale_fill_viridis(name = "Number of victims", 
#                        guide = guide_colorbar(barheight = unit(2, units = "mm"),
#                                               barwidth = unit(30, units = "mm"),
#                                               title.position = 'top', nrow = 1, 
#                                               ticks = FALSE),
#                        limits = c(0, 160)) + 
#     theme_void() + 
#     coord_map() + 
#     theme(plot.background = element_rect(fill = "#f5f5f2", color = NA), 
#           legend.position = c(0.15, 0.09), 
#           legend.direction = "horizontal", 
#           plot.caption = element_text(size = 6, color = "#4e4d47",
#                                       margin = margin(b = 0.3, unit = "cm"), hjust = 0.98), 
#           legend.title = element_text(size = 8, color = "#4e4d47"), 
#           legend.text = element_text(size = 7, color = "#4e4d47"), 
#           plot.title = element_text(size = 10, hjust = 0.01, color = "#4e4d47", 
#                                     margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")), 
#           plot.subtitle = element_text(size = 8, hjust = 0.006, color = "#4e4d47", 
#                                        margin = margin(b = -0.1, t = 0.25, l = 10, unit = "cm"))) -> mp
#   
#   file_out <- sprintf("mp-ms2-%s.png", yr)
#   ggsave(file_out, mp, width = 8, height = 5)
#   
#   file_out
# }
# 
# year <- levels(mass_shooting$year)
# 
# year %>% 
#   purrr::map(shooting_map) %>% 
#   purrr::map(image_read) %>% 
#   image_join() %>% 
#   image_animate(fps = 2) %>% 
#   image_write("shooting2.gif")
  


