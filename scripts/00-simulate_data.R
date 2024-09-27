#### Preamble ####
# Purpose: Simulates Code
# Author: Dong Jun Yoon
# Date: 26 September 2024 
# Contact: dongjun.yoon@mail.utoronto.ca
# License: MIT


#### Workspace setup ####
library(tidyverse)
library(kableExtra)
library(dplyr)
library(knitr)
library(ggplot2)

#### Simulate data ####
# data <- read.csv("ttc_bus_delay_data_2024.csv")

subset_data <- data %>%
  select(Date, Time, Day, Incident, Min_Delay, Min_Gap) %>% 
  head(10)

kable(subset_data, format = "pipe", align = 'l') %>%
  kable_styling(full_width = F, position = "center")

#data <- read.csv("ttc_streetcar_delay_data_2024.csv")

subset_data <- data %>%
  select(Date, Time, Day, Incident, Min_Delay, Min_Gap) %>% 
  head(10)

kable(subset_data, format = "pipe", align = 'l') %>%
  kable_styling(full_width = F, position = "center")

#data <- read.csv("ttc_subway_delay_data_2024.csv")

subset_data <- data %>%
  select(Date, Time, Day, Code, Min_Delay, Min_Gap) %>% 
  head(10)

kable(subset_data, format = "pipe", align = 'l') %>%
  kable_styling(full_width = F, position = "center")

#bus_data <- read.csv("ttc_bus_delay_data_2024.csv")
streetcar_data <- read.csv("ttc_streetcar_delay_data_2024.csv")

combined_data <- bind_rows(bus_data, streetcar_data)

incident_summary <- combined_data %>%
  group_by(Incident) %>%
  summarise(Total_Incidents = n()) %>%
  arrange(desc(Total_Incidents))
ggplot(incident_summary, aes(x = reorder(Incident, -Total_Incidents), y = Total_Incidents)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Reasons of Bus and Streetcar Delay", 
       x = "Incident Type", 
       y = "Total Incidents") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

#subway_data <- read.csv("Subway_count_2024.csv")

subway_data_clean <- subway_data %>%
  filter(!is.na(Delay.Group) & !is.na(Count) & Count > 0)

delay_group_summary <- subway_data_clean %>%
  group_by(Delay.Group) %>%
  summarise(Total_Count = sum(Count)) %>%  # Sum up the Count column for each Delay.Group
  arrange(desc(Total_Count))  # Arrange by total delays in descending order

ggplot(delay_group_summary, aes(x = reorder(Delay.Group, -Total_Count), y = Total_Count)) +
  geom_bar(stat = "identity", fill = "tomato") +
  labs(title = "Total Delays by Delay Group (TTC Subway)", 
       x = "Delay Group", 
       y = "Total Delays") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate x-axis labels for better readability

#library(ggplot2)
library(dplyr)

bus_data <- read.csv('ttc_bus_delay_data_2024.csv')
streetcar_data <- read.csv('ttc_streetcar_delay_data_2024.csv')
subway_data <- read.csv('ttc_subway_delay_data_2024.csv')

day_abbreviations <- c("Monday" = "Mon", "Tuesday" = "Tue", "Wednesday" = "Wed", 
                       "Thursday" = "Thu", "Friday" = "Fri", "Saturday" = "Sat", 
                       "Sunday" = "Sun")

abbreviate_day <- function(day) {
  return(day_abbreviations[day])
}

bus_delays_by_day <- bus_data %>%
  filter(Day %in% names(day_abbreviations)) %>%
  group_by(Day) %>%
  summarise(Count = n()) %>%
  mutate(Day = abbreviate_day(Day))

streetcar_delays_by_day <- streetcar_data %>%
  filter(Day %in% names(day_abbreviations)) %>%
  group_by(Day) %>%
  summarise(Count = n()) %>%
  mutate(Day = abbreviate_day(Day))

subway_delays_by_day <- subway_data %>%
  filter(Day %in% names(day_abbreviations)) %>%
  group_by(Day) %>%
  summarise(Count = n()) %>%
  mutate(Day = abbreviate_day(Day))

bus_delays_by_day$vehicle <- "Bus"
streetcar_delays_by_day$vehicle <- "Streetcar"
subway_delays_by_day$vehicle <- "Subway"

combined_data <- rbind(bus_delays_by_day, streetcar_delays_by_day, subway_delays_by_day)

ggplot(combined_data, aes(x = Day, y = Count, color = vehicle, group = vehicle)) +
  geom_line(size = 1) +
  geom_point() +
  labs(title = "Number of delays caused in day, by Vehicle",
       x = "Day of the Week",
       y = "Count of Delays",
       color = "Vehicle") +
  theme_minimal() +
  scale_x_discrete(limits = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))

#bus_data <- read.csv('ttc_bus_delay_data_2024.csv')
streetcar_data <- read.csv('ttc_streetcar_delay_data_2024.csv')
subway_data <- read.csv('ttc_subway_delay_data_2024.csv')

bus_summary <- bus_data %>%
  summarise(Total_Delays = n(), Avg_Delay_Time = mean(`Min_Delay`, na.rm = TRUE))

streetcar_summary <- streetcar_data %>%
  summarise(Total_Delays = n(), Avg_Delay_Time = mean(`Min_Delay`, na.rm = TRUE))

subway_summary <- subway_data %>%
  summarise(Total_Delays = n(), Avg_Delay_Time = mean(`Min_Delay`, na.rm = TRUE))

summary_data <- data.frame(
  Vehicle = c("Bus", "Streetcar", "Subway"),
  Total_Delays = c(bus_summary$Total_Delays, streetcar_summary$Total_Delays, subway_summary$Total_Delays),
  Avg_Delay_Time = c(bus_summary$Avg_Delay_Time, streetcar_summary$Avg_Delay_Time, subway_summary$Avg_Delay_Time)
)

ggplot(summary_data, aes(x = Vehicle, y = Total_Delays, fill = Vehicle)) +
  geom_bar(stat = "identity") +
  labs(title = "Total Number of Delays per Vehicle", x = "Vehicle", y = "Total Delays") +
  theme_minimal()

ggplot(summary_data, aes(x = Vehicle, y = Avg_Delay_Time, fill = Vehicle)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Delay Time per Vehicle (in Minutes)", x = "Vehicle", y = "Average Delay Time (Minutes)") +
  theme_minimal()



