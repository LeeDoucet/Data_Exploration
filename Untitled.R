
### PULL SOMETHING 

apartment_data <- 
  opendatatoronto::search_packages("Apartment Building Evaluation") %>%
  opendatatoronto::list_package_resources() %>%
  filter(name == "Apartment Building Evaluation") %>% #This is the row that we are interested in
  select(id) %>%
  opendatatoronto::get_resource()

### PLOT SOMETHING

ggplot(data = apartment_data, aes(x = WARD, y = SCORE)) +
  geom_bar(stat = "identity") +
  ylab("Scores (Out of 100)")+
  xlab("Ward Number") +
  ggtitle("          Apartment Evaluation Scores ")

test3 <-
  all_data %>%
  select(PCODE,SCORE)

test4<-  
  aggregate(. ~ PCODE, test3, mean) 
  
test5 <- aggregate(cbind(count = PCODE) ~ PCODE, 
                               data = test3, 
                               FUN = function(x){NROW(x)}) 

chart3 <- merge(test5,test4, by = "PCODE")

test6 <-
  raw_short_rentals %>%
  select(postal_code)

test7 <- aggregate(cbind(count = postal_code) ~ postal_code, 
                   data = test6, 
                   FUN = function(x){NROW(x)}) 
test7 <- 
  rename(test7, c("postal_code" = "PCODE", ))

test7<-  
  aggregate(. ~ postal_code, test6) 

chart3test <- merge(chart3, test7, by = "PCODE")
  




sum(chart3$count)
count(all_data$PCODE)

head(chart3)
apartment_data <- apartment_data %>%
  mutate(WARD = fct_reorder(WARD, SCORE))

chart3 %>%

test <- 
filter(chart3, `Average Area Apartment Score` > 80)


### COLUMNS 

apartment_data %>%
  select(SECURITY)

SCORE[SCORE < 50]

attach(Test) - detach(Test)

mean(Test$SCORE, na.rm = TRUE) #MEAN + REMOVE NA

sort(unique(Test$WARD))

names(raw_short_rentals)[names(raw_short_rentals) == "postal_code"] <- "PCODE"

apartment_eval2 <- apartment_eval2[-c(3460), ] 

table(Test$WARD) - data.frame(table(Test$WARD))

Test2 <- data.frame(table(Test$WARD, Test$SCORE))

aggregate(Test[, 25], list(Test$Name), mean)

all_data <- all_data[!is.na(all_data$SCORE),] 

test2 <- aggregate(test[8], 
                   list(Property_Type = test$PROPERTY_TYPE, Ward = test$WARD), mean) #Group by each category

### Change Values
Test$WARD <- as.numeric(Test$WARD)

sum(is.na(Test$SCORE))

USDA <- merge(Macro,Micro, by.x ="ID")

### group mtcars by cylinders and return some averages
cars <- mtcars %>%
  select(cyl, mpg, hp, qsec) %>%
  group_by(cyl) %>%
  summarise(mpg = mean(mpg), hp = mean(hp), qsec = mean(qsec))

by_cyl <- mtcars %>% group_by(cyl)

test <- test[!is.na(test$SCORE), ]

count(is.na(test$SECURITY))
test %>%
  mutate(total = SCORE + 100)
head(test,20)

data <- read.csv("https://raw.githubusercontent.com/guru99-edu/R-Programming/master/lahman-batting.csv") %>%
  select(c(playerID, yearID, AB, teamID, lgID, G, R, HR, SH))  %>% 
  arrange(playerID, teamID, yearID)

glimpse(data)

test <-
  all_data %>%
  select(PROP_MANAGEMENT_COMPANY_NAME, SCORE)


test2 <-  
  aggregate(. ~ PROPERTY_TYPE, test, mean) 




test<-  
  aggregate(. ~ all_data$PROP_MANAGEMENT_COMPANY_NAME, raw_apartment_eval, mean) 

test <-
  count(apartment_reg$PROP_MANAGEMENT_COMPANY_NAME)

test <-
data_chart2 %>%
  select(all_data$PROP_MANAGEMENT_COMPANY_NAME, %>%
  group_by(PROPERTY_TYPE) %>%
  count()



test <-
  data_chart2 %>%
  select(PROPERTY_TYPE, FIRE_ALARM) %>%
  group_by(PROPERTY_TYPE) %>%
  count()


value <- sum(test$freq)

test <-
  mutate(test, Percentage = freq / value)

test <-
mutate(test, Percentage = freq)

test %>%
filter(PROPERTY_TYPE) %>%

  summary(all_data)