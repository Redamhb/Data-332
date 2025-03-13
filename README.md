library(ggplot2)
library(dplyr)
library(tidyverse)
library(readxl)
library(tidyr)

rm(list = ls())

setwd("~/Documents/r_projects/trucking")

df_truck <- read_excel('NP_EX_1-2.xlsx', sheet = 2, skip =  3, .name_repair = 'universal') 

# selecting columns
df <- df_truck[, c(4:15)]
#removing columns
df = subset(df, select = -c(...10) )

#getting difference in days within a column
date1 <- min(df$Date)
date2 <- max(df$Date)

number_days_on_road <- date2 - date1

days <- difftime(min(df$Date), max(df$Date))

print(number_days_on_road)

#number of hours driving
num_hrs_driving <- sum(df$Hours)
print(num_hrs_driving)

number_of_days <- nrow(df)

average_time_driving <- round(num_hrs_driving / number_of_days, 3)

df$fuel_cost <- df$Gallons * df$Price.per.Gallon
# total
df$total_trip_cost <- df$Tolls + df$Misc + df$fuel_cost
# sum total
total_overall_cost <- sum(df$total_trip_cost)

#split

df[c('warehouse', 'starting_city_state')]<-
  str_split_fixed(df$Starting.Location,',',2)

#string extract
df$starting_city_state <- gsub(',', "", df$starting_city_state)
#Do another string split to show them what the problem is
df[c('col1', 'col2')] <-
  str_split_fixed(df$starting_city_state, '', 2)
#Do another string split to show them what the problem is

df[c('col1', 'col2')] <-
  str_split_fixed(df$starting_city_state,'', 2)

#Do this in console
df[c('col1', 'col2', 'col3')]<-
  str_split_fixed(df$col2, '', 3)

df_starting_Pivot <- df %>%
  group_by(starting_city_state) %>%
  summarize(count = n(),
            mean_size_hours = mean(Hours, na.rm = TRUE),
            sd_hours = sd(Hours, na.rm = TRUE),
            total_hours = sum(Hours, na.rm = TRUE),
            total_gallons = sum(Gallons, na.rm = TRUE)
  )
ggplot(df_starting_Pivot, aes(x = starting_city_state, y = count))+
  geom_col(fill = "blue")+
  theme(axis.text = element_text(angle = 45, vjust = .5, hjust = 1))


rm(list = ls())

setwd("~/Documents/r_projects/trucking")
df_truck_0001 <- read_excel('truck data 0001.xlsx', sheet = 2, skip = 3, .name_repair = 'universal')
df_truck_0369 <- read_excel('truck data 0369.xlsx', sheet = 2, skip = 3, .name_repair = 'universal')
df_truck_1226 <- read_excel('truck data 1226.xlsx', sheet = 2, skip = 3, .name_repair = 'universal')
df_truck_1442 <- read_excel('truck data 1442.xlsx', sheet = 2, skip = 3, .name_repair = 'universal')
df_truck_1478 <- read_excel('truck data 1478.xlsx', sheet = 2, skip = 3, .name_repair = 'universal')
df_truck_1539 <- read_excel('truck data 1539.xlsx', sheet = 2, skip = 3, .name_repair = 'universal')
df_truck_1769 <- read_excel('truck data 1769.xlsx', sheet = 2, skip = 3, .name_repair = 'universal')
df_pay <- read_excel('Driver Pay Sheet.xlsx', sheet = 2, .name_repair = 'universal')

df <- rbind(df_truck_0001, df_truck_0369, df_truck_1226, df_truck_1442, 
            df_truck_1478, df_truck_1539, df_truck_1769)

df_starting_Pivot <- df %>%
  group_by(Truck.ID) %>%
  summarize(count = n())

df <- left_join(df, df_pay, by = 'Truck.ID')



