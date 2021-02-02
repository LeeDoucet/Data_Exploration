#### Preamble ####
# Purpose: Use opendatatoronto to get Toronto dwelling data 
# Author: Lee Doucet
# Contact: Lee.Doucet@mail.utoronto.ca
# Date: 30 January 2021 
#Pre-requisites; None
# TODOs: - 


#### Workspace set-up ####
# install.packages("opendatatoronto")
library(opendatatoronto)
library(tidyverse)

### Question(s) ###

# 1) What are the differences between living in a private dwelling, TCH, or Social Housing 


#### Getting Data ####

## Apartment Evaluation
rawApartmentEval <- 
  opendatatoronto::search_packages("Apartment Building Evaluation") %>%
  opendatatoronto::list_package_resources() %>%
  filter(name == "Apartment Building Evaluation") %>% #This is the row that we are interested in
  select(id) %>%
  opendatatoronto::get_resource()

## Apartment Regulation
rawAartmentReg <- 
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

apartmentReg <- rawApartmenTReg
apartmentEval <- rawApartmentEval

apartment_reg <-
    apartment_reg %>%
    select(RSN, PCODE, AMENITIES_AVAILABLE, 
           APPROVED_FIRE_SAFETY_PLAN, FIRE_ALARM,
           PROP_MANAGEMENT_COMPANY_NAME)

apartment_eval <- 
  apartment_eval %>%
  select(RSN,CONFIRMED_UNITS, PROPERTY_TYPE, YEAR_BUILT, SECURITY,CONFIRMED_UNITS, WARD, SCORE)

all_data <- merge(apartment_reg,apartment_eval, by = "RSN")

all_data$SCORE <- as.numeric(all_data$SCORE)

all_data <- all_data[!is.na(all_data$SCORE),] 

data_chart1 <-
  all_data %>%
  select(PROPERTY_TYPE, SCORE) 

data <-
  raw_apartment_eval %>%
  select(CONFIRMED_UNITS,PROPERTY_TYPE)

data1 <-  
  aggregate(. ~ PROPERTY_TYPE, data, sum) 

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
  rename(chart3air, c("PCODE" = "postal_code"))

chart3pscore <-
  all_data %>%
  select(PCODE,SCORE)

chart3pavg <-
  aggregate(. ~ PCODE,chart3pscore, mean) 


chart3pcount <- aggregate(cbind(count = PCODE) ~ PCODE, 
                   data = all_data, 
                   FUN = function(x){NROW(x)}) 

chart3pscore <- merge(chart3pcount,chart3pavg, by = "PCODE")

summary(chart3pscore$SCORE)

chart3<- merge(chart3air, chart3pscore, by = "PCODE")

chart3 <- 
  rename(chart3, c("Number of Apartments" ="count.x", 
                   "Postal Code" = "PCODE", "Average Area Apartment Score" = "SCORE", 
                   "Number of Registered Airbnb's" ="count.y"))

chart3 <- chart3[, c(1, 2, 4,3)]

### Plot the Data ###

ggplot(data = all_data, aes(x = PROPERTY_TYPE, y = SCORE, fill = PROPERTY_TYPE)) +
  geom_boxplot() +
  ylab("Building Evaluation (%)") +
  ggtitle("                     Distribution Scores Between Apartment Types ")

attach(chart3pscore)
plot(count, SCORE, main="Scatterplot Example",
     xlab="Number of Rentals in Postal Code ", ylab="Score Per Rental ", pch=19) +
  abline(lm(SCORE~count), col="red") +
  lines(lowess(count,SCORE), col="blue")


#### Save Data ####
write_csv(all_data, "/Users/kerrybruner/Downloads/Hardcore Canadian Tire /School/UofT Winter 2021/Experimental Design/Project Test/Data_Exploration/raw_data.csv")


graph2Data <-
  allData %>%
  select(pCode,rentalScore) #Gathering data for the graph

graph2Avg <-
  aggregate(. ~ pCode,graph2Data, mean) #Getting Rental Score Average

graph2Count <- aggregate(cbind(count = pCode) ~ pCode, 
                         data = graph2Data, 
                         FUN = function(x){NROW(x)}) # Building Count Per Postal Code

graph2Combine <- merge(graph2Avg,graph2Count, by = "pCode") #Combination

graph2 <- 
  attach(graph2Combine)
plot(count, rentalScore, main="Relationship Between Average Rental Price and Postal Code",
     xlab="Number of Rentals in Postal Code ", ylab="Score Per Rental ", pch=19) +
  abline(lm(rentalScore~count), col="red") 
graph2



### Test code 

builtGraph <- 
  allData %>%
  select(propertyType,yearBuilt) 

sortBuiltGraph <-
  builtGraph %>%
  filter(builtGraph$yearBuilt > 1966) #range picked to make the data look more clear

sortBuiltGraph <-
  filter(sortBuiltGraph, propertyType == "SOCIAL HOUSING") 

builtGraph <-
  aggregate(sortBuiltGraph[,1:2], list(sortBuiltGraph$propertyType), mean)


ggplot(data=builtGraph, aes(x=yearBuilt, propertyType, group=1)) +
  geom_step(color="blue")+
  geom_point() +
  xlab("Year Built") +
  ylab("Number of Social Housing Units Built") +
  scale_color_grey() + theme_classic() +
  theme(legend.position="top") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


test12 <-
test1 %>%
  group_by(PROPERTY_TYPE, YEAR_BUILT) %>% 
  summarise_each(funs(length))

test12 <-
test12 %>%
  select(PROPERTY_TYPE, YEAR_BUILT) %>%
  mutate(total = mean(test12$YEAR_BUILT))

allData %>%
group_by(propertyType, yearBuilt) %>% 
  summarise_each(funs(length))


builtGraph <- 
  allData %>%
  select(propertyType,yearBuilt) 

sortBuiltGraph <-
  builtGraph %>%
  filter(builtGraph$yearBuilt > 1950)

sortBuiltGraph <-
  filter(sortBuiltGraph, propertyType == "SOCIAL HOUSING") 

builtGraph <-
  aggregate(. ~ yearBuilt,sortBuiltGraph, length)

  
ggplot(data=builtGraph, aes(x=yearBuilt, propertyType, group=1)) +
  geom_step(color="red")+
  geom_point() +
  xlab("Year Built") +
  ylab(" Number of Apartment Buildings Built") +
  scale_color_grey() + theme_classic() +
  theme(legend.position="top") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


stage2 <-
  aggregate(. ~ yearBuilt,test2, length)


stage2 <-
  stage2 %>%
  filter(stage2$yearBuilt > 1950)


summary(test2)

test3 <-
  filter(all_data, PROPERTY_TYPE == "TCHC") %>%
  select(PROPERTY_TYPE,YEAR_BUILT, SCORE) 

summary(test3)

test4 <-
  filter(all_data, PROPERTY_TYPE == "PRIVATE") %>%
  select(YEAR_BUILT, SCORE) 

summary(test4)

stage <-
  aggregate(. ~ YEAR_BUILT,test4, length)

stage <-
stage %>%
  filter(stage$YEAR_BUILT > 1950)

stage1 <-
  aggregate(. ~ YEAR_BUILT,test3, length)

stage1 <-
  stage1 %>%
  filter(stage1$YEAR_BUILT > 1950)






ggplot(data=stage, aes(x=YEAR_BUILT, PROPERTY_TYPE, group=1)) +
  geom_line()+
  geom_point()

ggplot(data=stage1, aes(x=YEAR_BUILT, PROPERTY_TYPE, group=1)) +
  geom_line()+
  geom_point()



library(ggplot2)
ggplot(data=test4, aes(x=YEAR_BUILT, SCORE, group=1)) +
  geom_line()+
  geom_point()

ACC <-
  rawApartmentReg %>%
  select(PROPERTY_TYPE,CONFIRMED_UNITS, NO_BARRIER_FREE_ACCESSBLE_UNITS) 

ACC <-
  ACC %>%
  filter(ACC$CONFIRMED_UNITS < 750)

  ACC$NO_BARRIER_FREE_ACCESSBLE_UNITS <- as.numeric(ACC$NO_BARRIER_FREE_ACCESSBLE_UNITS)
  
ACC <-  
    aggregate(. ~ PROPERTY_TYPE, ACC, sum) 

ACC <-
mutate(ACC, Percentage = CONFIRMED_UNITS / NO_BARRIER_FREE_ACCESSBLE_UNITS)

