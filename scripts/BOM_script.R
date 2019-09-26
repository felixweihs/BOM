library(tidyverse)
BOM_data <- read_csv("data/BOM_data.csv")
BOM_stations <- read_csv("data/BOM_stations.csv")

#Question 1 -
#For each station, how many days have a minimum temperature, a maximum temperature and a rainfall measurement recorded?

BOM_data 

BOM_data <- read_csv("data/BOM_data.csv") #Load data
separated_min_max <- separate(BOM_data, col = Temp_min_max, into = c('min', 'max'), sep = "/") #Separate temperatures into min and max
only_data <- separated_min_max %>% 
  filter(min != '-') %>% #Filter out min rows without value
  filter(max != '-') %>% #Filter out max rows without value
  filter(Rainfall != '0') %>% #Filter out rainfall
  group_by(Station_number) %>% #Group by Station numbers
  summarise(n = n()) #Count number of rows
write_csv(only_data, "results/question1.csv")
only_data

#Question 2 -
#Which month saw the lowest average daily temperature difference?

#Solution 1
separated_min_max
separated_min_max <- separate(BOM_data, col = Temp_min_max, into = c('min', 'max'), sep = "/") #Separate temperatures into min and max
avrg_temp_diff <- separated_min_max %>% 
  mutate(temp_diff = as.numeric(max) - as.numeric(min)) %>% #Calculate temperature difference using converted max and min values
  filter(temp_diff != 'NA') %>% #Filter out rows without valid data
  group_by(Month) %>% #Group by months
  summarise(avrg_temp_diff = mean(temp_diff)) %>% #Calculate average temperature difference
  arrange(avrg_temp_diff) #Arrange by lowest average daily temp difference
write_csv(avrg_temp_diff, "results/question2sol1.csv") #Output result
avrg_temp_diff

#Solution 2
avrg_temp_diff <- separated_min_max %>% 
  mutate(temp_diff = as.numeric(max) - as.numeric(min)) %>% #Calculate temperature difference using converted max and min values
  filter(min != '-') %>% #Filter out min rows without value
  filter(max != '-') %>% #Filter out max rows without value
  group_by(Month) %>% #Group by months
  summarise(avrg_temp_diff = mean(temp_diff)) %>% #Calculate average temperature difference
  arrange(avrg_temp_diff)#Arrange by lowest average daily temp difference
write_csv(avrg_temp_diff, "results/question2sol2.csv") #Output result
avrg_temp_diff

#Question 3 -
#Which state saw the lowest average daily temperature difference?
BOM_stations_tidy <- BOM_stations %>%  
  gather(Station_name, name, -info) %>% #tidy data intermediate
  filter(info == "state") %>%  #Only select rows with state
  select(info = Station_name, name) %>% #Only retain colums with station names and states
  mutate(info = as.numeric(info)) #Convert station names from character to double

separated_min_max <- separate(BOM_data, col = Temp_min_max, into = c('min', 'max'), sep = "/") #Separate temperatures into min and max
avrg_temp_diff_station <- separated_min_max %>% 
  mutate(temp_diff = as.numeric(max) - as.numeric(min)) %>% #Calculate temperature difference
  filter(temp_diff != 'NA') %>% #Filter out rows without 
  group_by(Station_number) %>% #Group by Station numbers
  summarise(avrg_temp_diff = mean(temp_diff)) %>% #Calculate average temperature difference
  rename(info = Station_number) #Rename Station_number for following joining function

BOM_stations_state_temp_diff <- inner_join(BOM_stations_tidy, avrg_temp_diff_station) %>% 
  group_by(name) %>% # Group by station name
  summarise(avrg_temp_diff_state = mean(avrg_temp_diff)) %>% #Calculate average temperature difference by state
  arrange(avrg_temp_diff_state)#Arrange by lowest average daily temp difference per state
  write_csv(avrg_temp_diff, "results/question3.csv") #Output result

#Question 4 -
# Does the westmost (lowest longitude) or eastmost (highest longitude) weather station in our dataset have a higher average solar exposure?

avrg_solar_exposure <- BOM_data %>% 
  filter(Solar_exposure != '-') %>% #Filter out out rows without solar exposure data
  mutate(Solar_exposure_Num = as.numeric(Solar_exposure)) %>% #Change solar exposure data into numeric values
  group_by(Station_number) %>% #Group by Station numbers
  summarise(avrg_solar_exposure = mean(Solar_exposure_Num)) #Calculate average temperature difference

write_csv(only_data, "results/question4.csv")

avrg_solar_exposure
BOM_data
