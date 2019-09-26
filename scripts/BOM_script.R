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
write_csv(only_data, "results/question1.csv")
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
write_csv(avrg_temp_diff, "results/question2sol1.csv")
avrg_temp_diff

#Solution 2
avrg_temp_diff <- separated_min_max %>% 
  mutate(temp_diff = as.numeric(max) - as.numeric(min)) %>% 
  filter(min != '-') %>% 
  filter(max != '-') %>% 
  group_by(Month) %>% 
  summarise(avrg_temp_diff = mean(temp_diff)) %>% 
  arrange(avrg_temp_diff)
write_csv(avrg_temp_diff, "results/question2sol2.csv")
avrg_temp_diff

#Question 3 -
#Which state saw the lowest average daily temperature difference?
BOM_stations_tidy <- BOM_stations %>%  
  gather(Station_name, name, -info) %>% #tidy data intermediate
  filter(info == "state") %>%  #Only select rows with state
  select(info = Station_name, name) %>% #Only retain colums with station names and states
  mutate(info = as.numeric(info)) #Convert station names from character to double



separated_min_max <- separate(BOM_data, col = Temp_min_max, into = c('min', 'max'), sep = "/") 
avrg_temp_diff_station <- separated_min_max %>% 
  mutate(temp_diff = as.numeric(max) - as.numeric(min)) %>% 
  filter(temp_diff != 'NA') %>% 
  group_by(Station_number) %>% 
  summarise(avrg_temp_diff = mean(temp_diff)) %>% 
  rename(info = Station_number) 


BOM_stations_state_temp_diff <- inner_join(BOM_stations_tidy, as.double(avrg_temp_diff_station))

BOM_stations_tidy
BOM_stations_state_temp_diff
BOM_stations
avrg_temp_diff_station


