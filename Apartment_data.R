#### Workspace set-up ####
# install.packages("opendatatoronto")
library(opendatatoronto)
library(tidyverse)


#TO_Do_List 
# Build Final Columns For Questions 

### Question(s) ###

# 1) What are the differences between living in a private dwelling, TCH, or Social Housing 


### Getting Data ###

## Apartment Evaluation
raw_apartment_eval <- 
  opendatatoronto::search_packages("Apartment Building Evaluation") %>%
  opendatatoronto::list_package_resources() %>%
  filter(name == "Apartment Building Evaluation") %>% #This is the row that we are interested in
  select(id) %>%
  opendatatoronto::get_resource()

## Apartment Regulation
raw_apartment_reg <- 
  opendatatoronto::search_packages("Building Registration") %>%
  opendatatoronto::list_package_resources() %>%
  filter(name == "Apartment Building Registration Data") %>% #This is the row that we are interested in
  select(id) %>%
  opendatatoronto::get_resource()

## Neighbourhood Profile 
raw_neighbourhood_profile <- 
  opendatatoronto::search_packages("Neighbourhood Profile") %>%
  opendatatoronto::list_package_resources() %>%
  filter(name == "neighbourhood-profiles-2016-csv") %>% #This is the row that we are interested in
  select(id) %>%
  opendatatoronto::get_resource()

## Development Applications Data
raw_development <-
  opendatatoronto::search_packages("Development Applications") %>%
  opendatatoronto::list_package_resources() %>%
  filter(name == "Development Applications Data") %>% #This is the row that we are interested in
  select(id) %>%
  opendatatoronto::get_resource()

## Short Term Rentals 
raw_short_rentals <- 
  opendatatoronto::search_packages("Short Term") %>%
  opendatatoronto::list_package_resources() %>%
  filter(name == "short-term-rental-registrations-data") %>% #This is the row that we are interested in
  select(id) %>%
  opendatatoronto::get_resource()

### Cleaning Evaluation Data ###

apartment_reg <- raw_apartment_reg
apartment_eval <- raw_apartment_eval

apartment_reg <-
    apartment_reg %>%
    select(RSN, PCODE, AMENITIES_AVAILABLE, 
           APPROVED_FIRE_SAFETY_PLAN, BALCONIES, 
           FIRE_ALARM, IS_THERE_EMERGENCY_POWER, NON_SMOKING_BUILDING, PROP_MANAGEMENT_COMPANY_NAME)

apartment_eval <- 
  apartment_eval %>%
  select(RSN,CONFIRMED_UNITS, PROPERTY_TYPE, YEAR_BUILT, SECURITY,CONFIRMED_UNITS, WARD, SCORE)

all_data <- merge(apartment_reg,apartment_eval, by = "RSN")

all_data$SCORE <- as.numeric(all_data$SCORE)

all_data <- all_data[!is.na(all_data$SCORE),] 

data_chart1 <-
  all_data %>%
  select(PROPERTY_TYPE, SCORE) 

data_chart2 <-
  all_data %>%
  select(PROPERTY_TYPE,APPROVED_FIRE_SAFETY_PLAN,FIRE_ALARM, SECURITY)

bar_data <-
  all_data %>%
  select(YEAR_BUILT, SCORE)

bar_data <-
  aggregate(. ~ YEAR_BUILT, bar_data, mean) 

### Aggregating Charts ###

#chart1 - Frequency Table + Average Means 

property_score <-  
  aggregate(. ~ PROPERTY_TYPE, data_chart1, mean) 

property_number <- aggregate(cbind(count = PROPERTY_TYPE) ~ PROPERTY_TYPE, 
                  data = all_data, 
                  FUN = function(x){NROW(x)})

chart1 <- merge(property_score,property_number, by = "PROPERTY_TYPE")

chart1 <- 
  rename(chart1, c("PROPERTY_TYPE"="Property Type", "SCORE" = "Score", "count" = "Numbers of Units"))

#chart2 - Difference in Safety Features

chart2plan <-
  data_chart2 %>%
  select(PROPERTY_TYPE, APPROVED_FIRE_SAFETY_PLAN) %>%
  group_by(PROPERTY_TYPE) %>%
  count()


chart2alarm <-
  data_chart2 %>%
  select(PROPERTY_TYPE, FIRE_ALARM) %>%
  group_by(PROPERTY_TYPE) %>%
  count()

chart2security <-
  data_chart2 %>%
  select(PROPERTY_TYPE, SECURITY) %>%
  group_by(PROPERTY_TYPE) %>%
  count()

## Chart 3 - Air BnB Time

chart3air <-
  raw_short_rentals %>%
  select(postal_code)

chart3air <- aggregate(cbind(count = postal_code) ~ postal_code, 
                   data = chart3air, 
                   FUN = function(x){NROW(x)}) 
chart3air <- 
  rename(chart3air, c("postal_code" = "PCODE"))

chart3pscore <-
  all_data %>%
  select(PCODE,SCORE)

chart3pavg <-
  aggregate(. ~ PCODE,chart3pscore, mean) 


chart3pcount <- aggregate(cbind(count = PCODE) ~ PCODE, 
                   data = all_data, 
                   FUN = function(x){NROW(x)}) 

chart3pscore <- merge(chart3pcount,chart3pavg, by = "PCODE")

chart3<- merge(chart3pscore, chart3air, by = "PCODE")

chart3 <- 
  rename(chart3, c("count.x" = "Number of Apartments", 
                   "PCODE" = "Postal Code", "SCORE" = "Average Area Apartment Score", 
                   "count.y" = "Number of Registered Airbnb's"))

chart3 <- chart3[, c(1, 2, 4,3)]

summary(test)

### Plot the Data ###

ggplot(data = all_data, aes(x = PROPERTY_TYPE, y = SCORE, fill = PROPERTY_TYPE)) +
  geom_boxplot() +
  ylab("Building Evaluation (%)") +
  ggtitle("                     Distribution Scores Between Apartment Types ")


ggplot(data = test, aes(x = Postal Code, y =SCORE)) +
  geom_bar(stat = "identity") +
  ylab("Scores (Out of 100)")+
  xlab("Ward Number") +
  ggtitle("          Apartment Evaluation Scores ")

