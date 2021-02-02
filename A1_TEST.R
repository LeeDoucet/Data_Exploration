#### Preamble ####
# Purpose: 
# Author: Lee Doucet
# Contact: Lee.Doucet@mail.utoronto.ca
# Date: 21 January 2021 
#Pre-requisites; None
# TODOs: - 


#### Workspace set-up ####
# install.packages("opendatatoronto")
library(opendatatoronto)
library(tidyverse)

#### Get Data ####

apartment_data <- 
  opendatatoronto::search_packages("apartment") %>%
  opendatatoronto::list_package_resources() %>%
  filter(name == "Apartment Building Evaluation") %>% #This is the row that we are interested in
  select(id) %>%
  opendatatoronto::get_resource()

ggplot(data = apartment_data, aes(x = WARD, y = SCORE)) +
  geom_bar(stat = "identity") +
  ylab("Scores (Out of 100)")+
  xlab("Ward Number") +
  ggtitle("          Apartment Evaluation Scores ")

apartment_data <- apartment_data %>%
  mutate(WARD = fct_reorder(WARD, SCORE))


  
all_data <- 
  opendatatoronto::search_packages("Rental") %>%
  opendatatoronto::list_package_resources() %>%
  filter(name == "short-term-rental-registrations-data") %>% #This is the row that we are interested in
  select(id) %>%
  opendatatoronto::get_resource()
 
reg_location <- all_data %>%
  drop_na(ward_number) %>%
  group_by(ward_number) %>%
  tally() %>%
  mutate(prop = n / sum(n))

ggplot(data = reg_location, aes(x = ward_number, y = prop)) +
  geom_bar(stat = "identity") +
  ylab("Proportion")+
  xlab("Ward Number") +
  ggtitle("          Proportion of Short Term Rental Registrations by Ward") 
 
reg_location <- reg_location %>%
  mutate(ward_number = fct_reorder(ward_number, prop))




#### Save Data ####
write_csv(all_data, "/Users/kerrybruner/Downloads/Hardcore Canadian Tire /School/UofT Winter 2021/Experimental Design/Project Test/Data_Exploration/raw_data.csv")


