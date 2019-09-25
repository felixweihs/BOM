library(tidyverse)
BOM_data <- read_csv("data/BOM_data.csv")
BOM_stations <- read_csv("data/BOM_stations.csv")

#Question 1 -
#For each station, how many days have a minimum temperature, a maximum temperature and a rainfall measurement recorded?

BOM_data 
separated_min_max <- separate(BOM_data, col = Temp_min_max, into = c('min', 'max'), sep = "/") 

only_data <- separated_min_max %>% 
  filter(min != '-') %>% 
  filter(max != '-') %>% 
  filter(Rainfall != '0') %>% 
  group_by(Station_number) %>% 
  summarise(n = n())
only_data

#Question 2 -
#Which month saw the lowest average daily temperature difference?

#Solution 1
separated_min_max
separated_min_max <- separate(BOM_data, col = Temp_min_max, into = c('min', 'max'), sep = "/") 
avrg_temp_diff <- separated_min_max %>% 
  mutate(temp_diff = as.numeric(max) - as.numeric(min)) %>% 
  filter(temp_diff != 'NA') %>% 
  group_by(Month) %>% 
  summarise(avrg_temp_diff = mean(temp_diff)) %>% 
  arrange(avrg_temp_diff)
avrg_temp_diff

#Solution 2
avrg_temp_diff <- separated_min_max %>% 
  mutate(temp_diff = as.numeric(max) - as.numeric(min)) %>% 
  filter(min != '-') %>% 
  filter(max != '-') %>% 
  group_by(Month) %>% 
  summarise(avrg_temp_diff = mean(temp_diff)) %>% 
  arrange(avrg_temp_diff)
avrg_temp_diff

#Question 3 -
#Which state saw the lowest average daily temperature difference?