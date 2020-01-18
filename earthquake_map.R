
library(RCurl)
library(shiny)
library(leaflet)
library(RColorBrewer)
library(jsonlite)


#create a function to grab and process the data sets.
#getQuakes takes a link to choose and loads the data corresponding to that link.
getQuakes<-function(link){
  #collect the data as a JSON file.
  data = jsonlite::fromJSON(link, flatten = TRUE)
  size = dim(data$features[1])[1]
  #create an empty dataframe.
  dataFrame = data.frame()
  #find the lat, long and mag from the json file.
  for (i in rep(1:size))
  {
    tempCoords = data$features$geometry.coordinates[[i]]
    record = data.frame(tempCoords[2], tempCoords[1], tempCoords[3], data$features$properties.mag[i])
    dataFrame = rbind(dataFrame, record)
  }
  #rename column names #return the created data frame.
  colnames(dataFrame) = c("lat", "long", "depth", "mag")
  #return the created data frame.
  return(dataFrame)
}
#making the default the last month.
dataFrame <- getQuakes(url("https://earthquake.usgs.gov/earthquakes/feed/v1.0/summary/4.5_month.geojson"))

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
                sliderInput("range", "Magnitudes", min(dataFrame$mag), max(dataFrame$mag),
                            value = range(dataFrame$mag), step = 0.1
                ),
                selectInput("colors", "Color Scheme",
                            rownames(subset(brewer.pal.info, category %in% c("seq", "div")))
                ),
                selectInput("url","Time Frame",c("last Month" = getURL("https://earthquake.usgs.gov/earthquakes/feed/v1.0/summary/4.5_month.geojson"),
                                                 "last Week" = getURL("https://earthquake.usgs.gov/earthquakes/feed/v1.0/summary/4.5_week.geojson") ,  
                                                 "last Day" = getURL("https://earthquake.usgs.gov/earthquakes/feed/v1.0/summary/4.5_day.geojson"),
                                                 "last Hour" = getURL("https://earthquake.usgs.gov/earthquakes/feed/v1.0/summary/all_hour.geojson"))
                            
                ),
                checkboxInput("legend", "Show legend", TRUE)
  )
)

server <- function(input, output, session) {
  
  # Reactive expression for the data subsetted to what the user selected
  filteredData <- reactive({
    getQuakes(input$url)[getQuakes(input$url)$mag >= input$range[1] & getQuakes(input$url)$mag <= input$range[2],]
  })
  
  # This reactive expression represents the palette function,
  # which changes as the user makes selections in UI.
  colorpal <- reactive({
    colorNumeric(input$colors, getQuakes(input$url)$mag)
  })
  
  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    leaflet(data = getQuakes(input$url)) %>% addTiles() %>%
      fitBounds(~min(long), ~min(lat), ~max(long), ~max(lat))
  })
  
  # Incremental changes to the map (in this case, replacing the
  # circles when a new color is chosen) should be performed in
  # an observer. Each independent set of things that can change
  # should be managed in its own observer.
  observe({
    pal <- colorpal()
    
    leafletProxy("map", data = filteredData()) %>%
      clearShapes() %>%
      addCircles(radius = ~10^mag/10, weight = 1, color = "#777777",
                 fillColor = ~pal(mag), fillOpacity = 0.7, popup = ~paste(mag)
      )
  })
  
  # Use a separate observer to recreate the legend as needed.
  observe({
    proxy <- leafletProxy("map", data = getQuakes(input$url))
    
    # Remove any existing legend, and only if the legend is  
    # enabled, create a new one.
    proxy %>% clearControls()
    if (input$legend) {
      pal <- colorpal()
      proxy %>% addLegend(position = "bottomright",
                          pal = pal, values = ~mag
      )
    }
  })
  observe({
    val <- input$range
    # Control the value, min, max, and step.
    # Step size is 2 when input value is even; 1 when value is odd.
    updateSliderInput(session, "range",
                      min = min(getQuakes(input$url)$mag), max = max(getQuakes(input$url)$mag))
  })
  
}

shinyApp(ui, server)