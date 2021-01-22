#### Preamble ####
# Purpose: Use opendatatoronto get get homelessness data
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

all_data <- 
  opendatatoronto::search_packages("Daily Shelter Occupancy") %>%
  opendatatoronto::list_package_resources() %>%
  filter(name == "daily-shelter-occupancy-2017-csv") %>% #This is the row that we are interested in
  select(id) %>%
  opendatatoronto::get_resource()
  
#### Save Data ####
write_csv(all_data, "/Users/kerrybruner/Downloads/Hardcore Canadian Tire /School/UofT Winter 2021/Experimental Design/Project Test/Data_Exploration/raw_data.csv")


