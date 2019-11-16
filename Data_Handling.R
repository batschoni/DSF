
###############################
#Data Handling#################
#remove duplicates and keep values which are the newest

library(tidyverse)
rm(list = ls())
data = read.csv("Data/Data_food_inspections.csv")
dem_data = read.csv("Data/Demographic_Statistics_By_Zip_Code.csv")
dem_data2 = read.csv("Data/Demographics_By_Zip_code.csv")


data = as_tibble(data)
data = data %>% 
  mutate(Inspection.Date = as.Date(data$Inspection.Date, format = "%m/%d/%Y")) %>% #convert values to dates for calculation
  mutate(id = row_number()) %>% #add row number as id
  filter(data$Trade.Name != "") %>% #remove rows with Trade.Name = ""
  arrange(Inspection.Date) #sort by date

data_unique = data %>%  #keep only the newest duplicates; possible with last function, because sorted above by date
  group_by(Trade.Name) %>% 
  summarise_all(funs(last)) %>% 
  ungroup

write.csv(data_unique, file = "Data/Unique_data_food_inspections.csv") #save data as csv


#IF! only for City of New York -> here a join of the demographic data with original data
dem_data = rename(dem_data, Zip.Code = JURISDICTION.NAME)
joined = inner_join(county_ny, dem_data, by = NULL)
joined2 = inner_join(data, dem_data2, by = NULL)

data = data %>% 
  group_by(Inspection.Grade) %>% 
  summarise(n = n())