# Avi Ahuja # UCID: 12208018
# Data Skills 2 - R
# Winter Quarter 2021
## Homework 2 - Question 3##

library(tidyverse)
library(shiny)
library(sf)
library(spData)
library(plotly)
library(scales)

#  Output objects saved to output$<name>
# Output object must be a render function
# Access input values with input$<name>


ui <- fluidPage(
  fluidRow(
    column(width = 6,
      selectInput(inputId = "zip",
                  label = "Choose a Zip Code",
                  c("60647", "60639", "60707", "60622", "60651",
                    "60611", "60638", "60652", "60626", "60615",
                    "60621", "60645", "60643", "60660", "60640",
                    "60614", "60631", "60646", "60628", "60625",
                    "60641", "60657", "60636", "60649", "60617",
                    "60633", "60612", "60604", "60624", "60656",
                    "60644", "60655", "60603", "60605", "60653",
                    "60609", "60666","60618", "60616", "60602",
                    "60601", "60608", "60607", "60661", "60606",
                    "60827", "60630", "60642", "60659", "60634",
                    "60613", "60610", "60654", "60632", "60623",
                    "60629", "60620", "60637", "60619"),
                  multiple = FALSE)
    )
  ),
  column(width = 6,
    checkboxInput(inputId = "street_toggle", 
                  label = "Street View",
                  value = FALSE)
  )
,    #need to change the name of this to zip codes
  plotOutput("map")     
) 

server <- function(input, output) {
  path <- "~/Documents/GitHub/Data and Programming in R II/data_skills_2_r_hw2/"  #loading step
  df <- read_sf(paste0(path, "death_count.csv"))  
  chicago_zip <- st_read(paste0(path, "Boundaries - ZIP Codes/geo_export_40257423-bd60-4f23-a8b2-272a7aa56f4a.shp"))
  chicago_streets <- st_read(paste0(path, "Major_Streets/Major_Streets.shp"))
  df <- right_join(chicago_zip, df, by = "zip")
  
  #Subsetting the data
  data <- reactive({
    filter(df, zip == input$zip)
  })
  
  # Create the map
  output$map <- renderPlot({
    if(input$street_toggle == TRUE) { 
    ggplot() +  
      geom_sf(data = chicago_zip) + 
      geom_sf(data = chicago_streets) +
      geom_sf(data = data(), aes(fill = cnt_total)) +
      labs(title = "COVID-19 Total Death Count by Zip Code (Chicago)", 
           fill = element_blank(),    
           caption = "Source: City of Chicago Data Portal") +
      theme_void() 
    }
    else{ 
      ggplot() +  
      geom_sf(data = chicago_zip) + 
      geom_sf(data = data(), aes(fill = cnt_total)) +
      labs(title = "COVID-19 Total Death Count by Zip Code (Chicago)", 
           fill = element_blank(),    
           caption = "Source: City of Chicago Data Portal") +
      theme_void() 
    }
      #scale_fill_gradient(low = "white", high = "dark red")
  })
  
}

shinyApp(ui = ui, server = server)
  
