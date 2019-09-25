library(tidyverse)
BOM_data <- read_csv("data/BOM_data.csv")
BOM_stations <- read_csv("data/BOM_stations.csv")

#Question 1 -
#For each station, how many days have a minimum temperature, a maximum temperature and a rainfall measurement recorded?

BOM_data 
separated_min_max <- separate(BOM_data, col = Temp_min_max, into = c('min', 'max'), sep = "/") 

only_min <- separated_min_max %>% filter(min != '-') %>% 
  group_by(Station_number)
nrow(only_min)

only_max <- separated_min_max %>% filter(max != '-') %>% 
  group_by(Station_number) %>% summary()
only_max

only_rainfall <- separated_min_max %>% filter(Rainfall != '-') %>% 
  group_by(Station_number) %>% summary()         
only_rainfall


separated_min_max

