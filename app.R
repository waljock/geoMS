library(ggplot2)
library(tidyverse)
library(readxl)
library(leaflet)
library(htmlwidgets)
library(shiny)
library(shinythemes)



s <- read_excel("GEO_DLR_MScan.xlsx")

s$MODEL_YEAR <- as.character(s$MODEL_YEAR)
grouped_Series <- unique(subset(s[c(4)],(s$MODEL_YEAR>="2018") & (s$ADI_DS_UNITS >= 1)))
grouped_MY <- (unique(subset(s[c(1)],(s$MODEL_YEAR>="2018") & (s$ADI_DS_UNITS >= 1))))
r <- data.frame(s[rep(row.names(s), s$ADI_DS_UNITS),])



# This is the Shiny UI
ui <- bootstrapPage(
  
		      tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
		        
		        absolutePanel(top = 10, right = 10,
				                      selectInput(inputId = "year", label = strong("MY"),
								                              choices = grouped_MY$MODEL_YEAR,
											                                  selected = "2018"),
				                      selectInput(inputId = "series", label = strong("Series"),
								                              choices = grouped_Series$SERIES_CD,
											                      selected = "1")),
	leafletOutput("map", width = "100%", height = "100%")

			)
  
# Define server function

server <- function(input, output, session) {
#reactive here allow pulldowns to update the map
  
	  filteredData <- reactive(data.frame((subset(r,(SERIES_CD==input$series) & (MODEL_YEAR == input$year)))))
  
  
 
  
  output$map <- renderLeaflet({
	      # Use leaflet() here, and only include aspects of the map that
	      # won't need to change dynamically (at least, not unless the
	      # entire map is being torn down and recreated).
	      leaflet(map) %>% addTiles() %>%
		            addMarkers(data=filteredData(), label =~ DEALER_CD, clusterOptions = markerClusterOptions())
		          
		      })
}

  

  
#Run the app 

shinyApp(ui = ui, server = server)               
  

