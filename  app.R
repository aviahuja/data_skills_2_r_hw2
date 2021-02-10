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

path <- "~/Documents/GitHub/Data and Programming in R II/data_skills_2_r_hw2/"  #loading step
og_df <- read_csv(paste0(path, "death_count.csv"))
og_df <- select(og_df, c("zip"))

ui <- fluidPage(
  varSelectInput(inputId = "zip",
              label = "Choose a Zip Code",
              og_df,
              multiple = TRUE),    #need to change the name of this to zip codes
  plotOutput("map"),     
  tableOutput("zip_disp")
)

server <- function(input, output) {
  path <- "~/Documents/GitHub/Data and Programming in R II/data_skills_2_r_hw2/"  #loading step
  df <- read_csv(paste0(path, "death_count.csv"))  
  shape_file <- st_read(paste0(path, "geo_export_40257423-bd60-4f23-a8b2-272a7aa56f4a.shp"))
  
  #Subsetting the data
  data <- reactive({
    filter(df, zip == input$zip)
  })
  
  
  # Create the map
  output$map <- renderPlot({    
    ggplot() +  
      geom_sf(data = shape_file) + 
      geom_sf(data = data(), aes(fill = !!cnt_total)) +
      labs(title = "COVID-19 Total Death Count by Zip Code (Chicago)", 
           fill = element_blank(),    #if I want no caption above my fill
           caption = "Source: City of Chicago Data Portal") +
      theme_void() +
      scale_fill_gradient(low = "white", high = "dark red")
    
  })
  
  # Display Table    
  output$zip_disp <- renderTable({   #notice for the table we assign the output $ + whataver name we gave to the table in the input function
    ggplot(data = data()) +
      d <- data()
      d
  })

}

shinyApp(ui = ui, server = server)
  
