
library(tidyverse)
library(httr)

# pulling data

mass_shooting <- 
  read_csv("https://raw.githubusercontent.com/StanfordGeospatialCenter/MSA/master/Data/Stanford_MSA_Database.csv")

# getting US geo data

us <- maps::map_data("usa")
