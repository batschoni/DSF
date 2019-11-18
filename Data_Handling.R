
###############################
#Data Handling#################
#remove duplicates and keep values which are the newest

library(tidyverse)
rm(list = ls())
data = read.csv("Data/Data_food_inspections.csv")


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

data_chains = data_unique %>% 
  group_by(Owner.Name) %>%  #group by owner to see who owns more than one company
  summarise(count = n())

data_chains$count[which(data_chains$count == 1)] = 0 #gives every owner the value 0 if only one shop owned
data_chains$count[which(data_chains$count != 0)] = 1 #gives value 1 if > 1 shop owned

data_unique = inner_join(data_unique, data_chains, by = "Owner.Name") #merges the data to basic data set


write.csv(data_unique, file = "Data/Unique_data_food_inspections.csv") #save data as csv
