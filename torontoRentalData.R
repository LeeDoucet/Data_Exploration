#### Preamble ####
# Purpose: Use opendatatoronto to get Toronto dwelling data 
# Author: Lee Doucet
# Contact: Lee.Doucet@mail.utoronto.ca
# Date: 30 January 2021 

#### Workspace set-up ####

# install.packages("opendatatoronto")
# install.packages("opendatatoronto")
# install.packages('knitr', dependencies = TRUE)
# install.packages("kableExtra")
# install.packages("bibtex")
library(bibtex)
library(opendatatoronto)
library(tidyverse)
library(kableExtra)

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

#### Organizing the Data ####

apartmentReg <- rawApartmentReg
apartmentEval <- rawApartmentEval

apartmentReg <-
  apartmentReg %>%        #Removing unessential columns
  select(RSN, PCODE,APPROVED_FIRE_SAFETY_PLAN, FIRE_ALARM,
         IS_THERE_EMERGENCY_POWER, NO_BARRIER_FREE_ACCESSBLE_UNITS,
         PROP_MANAGEMENT_COMPANY_NAME)

apartmentEval <- 
  apartmentEval %>%
  select(RSN,CONFIRMED_UNITS, PROPERTY_TYPE, YEAR_BUILT,
         SECURITY,CONFIRMED_UNITS, WARD, SCORE)

allData <- merge(apartmentReg,apartmentEval, by = "RSN") #Common identifier in both data sets 

allData$SCORE <- as.numeric(allData$SCORE) # Making Apartment Score numeric

allData <- allData[!is.na(allData$SCORE),] #Removing 13 NA's 

allData <-       #Cleaning up names too long and full of CAPS
  rename(allData, c("pCode" = "PCODE", "firePlan" = "APPROVED_FIRE_SAFETY_PLAN", 
                    "fireAlarm" = "FIRE_ALARM", "rentalScore" = "SCORE",
                    "emergPower" = "IS_THERE_EMERGENCY_POWER",
                    "rentalUnits" = "CONFIRMED_UNITS",
                    "propertyManager" = "PROP_MANAGEMENT_COMPANY_NAME",
                    "propertyType" = "PROPERTY_TYPE", "yearBuilt" = "YEAR_BUILT",
                    "security" = "SECURITY", "ward" = "WARD", "rsn" = "RSN",
                    "accessUnits" ="NO_BARRIER_FREE_ACCESSBLE_UNITS" ))


#### Data Preperation for Chart 1 Comparing Rentals By Property Types ####

chart1 <-
  allData %>%
  select(propertyType, rentalScore) #Want to compare these two columns 

property1 <-  #Average Rental Score of each property type 
  aggregate(. ~ propertyType, chart1, mean) 

property2 <- aggregate(cbind(count = propertyType) ~ propertyType, 
                       data = allData, 
                       FUN = function(x){NROW(x)}) # Sum Property Type

chart1 <- merge(property1,property2, by = "propertyType") 

chart1 <- #Getting Proper Names 
  rename(chart1, c("Property Type" = "propertyType", 
                   "Average Rental Score" = "rentalScore",
                   "Total Amount" = "count"))



#### Graph 1 - Box Plot of Property Type Distributions ####

graph1 <- 
  ggplot(data = allData, aes(x = propertyType, y = rentalScore, fill = propertyType)) +
  geom_boxplot() +
  ylab("Building Evaluation (%)") +
  xlab("Property Type") +
  ggtitle("Graph 1 - Distribution Scores Between Apartment Types ") + 
  labs(caption = "(Based on data from ggPlot2 (Wickham 2016) & R (R Core Team 2020)") +
  scale_fill_discrete(name = "Property Type")
graph1

#### Summary Chart for Property Type / Rental Score Totals ####

chart1 %>% #Creating a chart to display the data for Property Type by Score/Count
  knitr::kable(caption = "Summary statistics", digits = 2) %>%  # Reducing decimal places 
  kableExtra::kable_styling(bootstrap_options = "striped", full_width =  FALSE)

#### Two Line Graphs - Showcasing the decline of Social Housing, esp. since 90's ####

builtGraph <- 
  allData %>%
  select(propertyType,yearBuilt) 

sortBuiltGraph <-
  builtGraph %>%
  filter(builtGraph$yearBuilt > 1966) #range picked to make the data look more clear

sortBuiltGraph <-
  filter(sortBuiltGraph, propertyType == "SOCIAL HOUSING") 

builtGraph <-
  aggregate(. ~ yearBuilt,sortBuiltGraph, length)


ggplot(data=builtGraph, aes(x=yearBuilt, propertyType, group=1)) +
  geom_step(color="blue")+
  geom_point() +
  xlab("Year Built") +
  ylab("Number of Social Housing Units Built") +
  scale_color_grey() + theme_classic() +
  theme(legend.position="top") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("                     Graph 3 - Decline of Social Housing Being Built") +
  labs(caption = "(Based on data from ggPlot2 (Wickham 2016) & R (R Core Team 2020)") 

builtGraph2 <- 
  allData %>%
  select(propertyType,yearBuilt) 

sortBuiltGraph2 <-
  builtGraph2 %>%
  filter(builtGraph2$yearBuilt > 1966) #range picked to make the data look more clear

sortBuiltGraph2 <-
  filter(sortBuiltGraph2, propertyType == "TCHC") 

builtGraph2 <-
  aggregate(. ~ yearBuilt,sortBuiltGraph2, length)


ggplot(data=builtGraph2, aes(x=yearBuilt, propertyType, group=1)) +
  geom_step(color="red")+
  geom_point() +
  xlab("Year Built") +
  ylab(" Number of TCHC Rental Apartments Built") +
  scale_color_grey() + theme_classic() +
  theme(legend.position="top") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("                  Graph 3 - Decline of TCHC Rentals Being Built ") +
  labs(caption = "(Based on data from ggPlot2 (Wickham 2016) & R (R Core Team 2020)") 

### Chart 2 - Comparing Accessibility Access in Rental Units between Property Types ####
chart2Data <-
  allData %>%
  select(propertyType,rentalUnits, accessUnits)

chart2Data <-
  chart2Data %>%
  filter(rentalUnits < 750)

chart2Data$accessUnits <- as.numeric(chart2Data$accessUnits)
chart2Data$rentalUnits <- as.numeric(chart2Data$rentalUnits)

chart2Agg <-  
  aggregate(. ~ propertyType, chart2Data, sum) 

chart2 <-
  mutate(chart2Agg, Percentage = accessUnits / rentalUnits * 100)

chart2 <-
  rename(chart2, c("Property Type" = "propertyType",
                   "Rental Units" = "rentalUnits",
                   "Accessibility Units" = "accessUnits"))

chart2 %>% 
  knitr::kable(caption = "Summary statistics", digits = 2) %>%
  kableExtra::kable_styling(bootstrap_options = "striped", full_width =  FALSE)

#### Save Data ####
write_csv(allData, "/Users/kerrybruner/Downloads/Hardcore Canadian Tire /School/UofT Winter 2021/Experimental Design/Project Test/Data_Exploration/raw_data.csv")