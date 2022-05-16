library("skimr")
library("janitor")
library("tidyverse")
library("lubridate")
library("dplyr")
library(readr)
library(readr)
cities <- read_csv("AIR QUALITY INDEX (by cities) - IQAir.csv", 
                                              col_types = cols(`2017` = col_number(), 
                                                               `2018` = col_number(), `2019` = col_number(), 
                                                               `2020` = col_number(), `2021` = col_number(), 
                                                               `JAN(2021)` = col_number(), `FEB(2021)` = col_number(), 
                                                               `MAR(2021)` = col_number(), `APR(2021)` = col_number(), 
                                                               `MAY(2021)` = col_number(), `JUN(2021)` = col_number(), 
                                                               `JUL(2021)` = col_number(), `AUG(2021)` = col_number(), 
                                                               `SEP(2021)` = col_number(), `OCT(2021)` = col_number(), 
                                                               `NOV(2021)` = col_number(), `DEC(2021)` = col_number()))

library(readr)
top_country <- read_csv("AIR QUALITY INDEX- top countries.csv", 
                                            col_types = cols(`2018` = col_double(), 
                                                             `2019` = col_double(), `2020` = col_double(), 
                                                             `2021` = col_double()))
## Number of rows and column in both country and city dataset
dim(top_countries)
dim(cities)


dir("Air Quality",full.names = T)
##seperaty the city column into city and country
#New_city <- separate(cities,City,into=c ('city','country'), sep=',')
##Note additional pieces discarded in 1 row (6234)

##rename the error in row 6234 to the correct name+

#New_city[New_city == " Bonaire"] <- "Bonaire, Saint Eustatius and Saba"
#glimpse(New_city)
###find what % of NA are in each column for the country and cities dataset
mean_country <- top_country %>% 
  select(- c("Rank" , `Country/Region` ,)) %>% 
  group_by() %>% 
  mutate(across(everything(), is.na)) %>% 
  summarize(across(everything(), mean))
##
mean_city <- cities %>% 
  select(- c("Rank" , 'City' , )) %>% 
  group_by() %>% 
  mutate(across(everything(), is.na)) %>% 
  summarize(across(everything(), mean))
## Replace the NA a values with the mean in the country dataset
updated_top_counties  <- top_country %>%
  mutate_if(is.numeric, ~replace_na(.,mean(., na.rm = TRUE)))

## Replace the NA a values with the mean in the city dataset
updated_New_city  <- cities %>%
  mutate_if(is.numeric, ~replace_na(.,mean(., na.rm = TRUE)))

write.csv(updated_top_counties, "Country Air quality.csv")
write.csv(updated_New_city, "City Air quality.csv")
