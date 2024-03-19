library(shiny)
library(leaflet)
library(dplyr)
library(sf)
library(readxl)
library(RColorBrewer)

# Define UI
ui <- fluidPage(
  titlePanel("Singapore Regions Visualization"),
  sidebarLayout(
    sidebarPanel(
      selectInput("variable", "Choose a variable to display:",
                  choices = c("Total Households" = "Total_Households",
                              "Mean Income" = "Mean_Income", 
                              "Occupation Score" = "Occupation_Score", 
                              "Education Score" = "Education_Score"))
    ),
    mainPanel(
      leafletOutput("map", height = 800) # Adjust height as needed
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Load and prepare the shapefile
  shapefile_path <- "C:/AlexeiJason/WHRshiny/data/Singapore.osm.shp/Singapore-shp/shape"
  sg_shape <- st_read(shapefile_path, quiet = TRUE)
  
  # Define the path to your Excel data
  data_path <- "C:/AlexeiJason/WHRshiny/data/SingaporeData.xlsx"
  # Read your Excel data
  data <- read_excel(data_path) 
  
  # Reactive expression to create merged data
  merged_data_reactive <- reactive({
    merged_data <- merge(sg_shape, data, by.x = "name", by.y = "Planning Area of Residence", all.x = TRUE)
    centroids <- st_centroid(merged_data)
    merged_data$lat <- st_coordinates(centroids)[,2]
    merged_data$lng <- st_coordinates(centroids)[,1]
    # Clean up merged data, handle missing values
    merged_data[is.na(merged_data)] <- 0  # Replace NA with 0 for missing values
    return(merged_data)
  })
  
  # A named list of color palettes
  colorPalettes <- list(
    Total_Households = "Blues",
    Mean_Income = "Greens",
    Occupation_Score = "Reds",
    Education_Score = "Purples"
  )
  
  # A reactive expression for the color palette based on the selected input
  colorpal <- reactive({
    var <- input$variable
    domain_data <- merged_data_reactive()[[var]]
    colorNumeric(palette = colorPalettes[[var]], domain = domain_data, na.color = "transparent")
  })
  
  # Render the Leaflet map
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = 103.851959, lat = 1.290270, zoom = 11) # Center the map on Singapore
  })
  
  # Observe block to update the map based on the input
  observe({
    var <- input$variable
    pal <- colorpal()
    md <- merged_data_reactive()
    
    leafletProxy("map", data = md) %>%
      clearShapes() %>%
      addPolygons(data = md,
                  fillColor = ~pal(md[[var]]),
                  fillOpacity = 0.7,
                  weight = 1,
                  color = "#BDBDC3",
                  popup = ~paste("<b>", name, "</b><br>", var, ": ", md[[var]]))
  })
  
  # Observe block to update the legend based on the input
  observe({
    var <- input$variable
    pal <- colorpal()
    md <- merged_data_reactive()
    
    leafletProxy("map") %>%
      clearControls() %>%
      addLegend(position = "bottomright",
                pal = pal,
                values = ~md[[var]],
                title = var,
                labFormat = labelFormat(transform = round),
                opacity = 1)
  })
}

# Run the app
shinyApp(ui, server)
