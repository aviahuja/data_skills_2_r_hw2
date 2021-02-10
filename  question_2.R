# Avi Ahuja # UCID: 12208018
# Data Skills 2 - R
# Winter Quarter 2021
## Homework 2 - Question 2 ##

#loading relevant libraries
library(tidyverse)
library(sf)
library(spData)

# Question 2 #

#setwd("~/Documents/GitHub/Data and Programming in R II/data_skills_2_r_hw2")  #Customize according user

path <- "~/Documents/GitHub/Data and Programming in R II/data_skills_2_r_hw2/"
chicago_zip <- st_read(paste0(path, "Boundaries - ZIP Codes/geo_export_40257423-bd60-4f23-a8b2-272a7aa56f4a.shp")) #59 unique zips

# Loading the two datasets

fire_stations <- read_csv("Fire_Stations.csv")    #50 unique zip codes

covid_cases <- read_csv("COVID-19_Cases__Tests__and_Deaths_by_ZIP_Code.csv")  #60 unique zip codes
covid_deaths <- select(covid_cases, c("ZIP Code", "Week Number", "Deaths - Weekly", "Deaths - Cumulative"))  #keeping only the deaths
covid_deaths <- filter(covid_deaths, !grepl("Unknown", `ZIP Code`)) #dropping the Unknown zip codes #59 unique zip codes
#senior_centers <-read_csv("Senior_Centers.csv")


######################## 1. FIRE STATION PLOT #########################

# Merging shape file with the fire station data
fire_stations <- rename(fire_stations, "zip" = "ZIP")
fire_stations <- fire_stations %>%  
  mutate(zip = as.factor(zip))
chicago_zip_fire <- right_join(chicago_zip, fire_stations, by = "zip")
# 50 unique zip codes

# Counting the number of fire stations per zip code
fire_count <- chicago_zip_fire %>%
  group_by(zip) %>%
  summarise(cnt = n()) %>%
  mutate(cnt = cnt)

#Plotting Chloropleth 1 - Fire Station Density by Zip Code 

chloropleth1 <- ggplot() +
  geom_sf(data = fire_count, aes(fill = cnt)) +
  labs(title = "Fire Station Density by Zip Code (Chicago)", 
       fill = element_blank(),    
       caption = "Source: City of Chicago Data Portal") +
  theme_void() +
  scale_fill_gradient(low = "white", high = "dark green")

## This plot displays the density of fire stations in the city of Chicago by zip code. Lighter colors on the plot
## correspond to zip codes with a lower density of fire stations, and darker colors correspond to a greater density of 
## fire stations. This plot can help map the fire-accident risk of each zip code in the city

######################## 2. COVID DEATHS PLOT #########################

# Counting the total number of covid deaths per zip code
covid_deaths <- rename(covid_deaths, "zip" = "ZIP Code" )
death_count = covid_deaths %>%                          
  group_by(zip) %>%
  summarise(cnt_total = sum(`Deaths - Weekly`, na.rm = TRUE)) 

write.csv(death_count,'death_count.csv')   #saving for usage in Q3
 
# Merging shape file with the covid death counts data
death_count <- right_join(chicago_zip, death_count, by = "zip")
st_write(death_count, 'death_count_sf.csv', layer_options = "GEOMETRY=AS_WKT")   #Outputing SF as dataframe for Q3
#Plotting Chloropleth 2 - Total COVID death count by Zip Code

chloropleth2 <- ggplot() +
  geom_sf(data = death_count, aes(fill = cnt_total)) +
  labs(title = "COVID-19 Total Death Count by Zip Code (Chicago)", 
       fill = element_blank(),    #if I want no caption above my fill
       caption = "Source: City of Chicago Data Portal") +
  theme_void() +
  scale_fill_gradient(low = "white", high = "dark red")

## This plot displays the total COVID-19total death counts in the city of Chicago by zip code. Lighter colors on the plot
## correspond to zip codes with a lower total death count, and darker colors correspond to a greater total death count.
## This plot can help prepare for the future intensity of COVID-19 related risks in each zip code of the city of Chicago.

