pacman::p_load(shiny, shinydashboard, dplyr, tidyr, ggplot2, highcharter, shinyWidgets, 
               data.table, stringr, plotly, tidyverse, glue, corrplot, MASS, ggpubr, car, fontawesome)

SGdata = read.csv("data/SingaporeData.csv")

ui <- fluidPage(
  titlePanel("Singapore Data Explorer"),
  
  # Map and Bar Chart side by side
  fluidRow(
    column(width = 6,
           leafletOutput("map")
    ),
    
    column(width = 6,
           selectInput("variable", "Select Variable",
                       choices = c("Total.Households","Mean.Income", "Occupation.Score", "Education.Score")),
           plotlyOutput("barChart"),
           selectInput("areaFilter", "Select Planning Area",
                       choices = unique(SGdata$Planning.Area.of.Residence)),
           tableOutput("areaValues")
    )
  )
)

server <- function(input, output, session) {
  
  output$map <- renderLeaflet({
    leaflet(SGdata) %>%
      addTiles() %>%
      setView(lng = 103.8198, lat = 1.3521, zoom = 11) %>%
      addMarkers(
        lng = ~Longitude, 
        lat = ~Latitude,
        popup = ~paste("Planning Area: ", Planning.Area.of.Residence, "<br>",
                       "Total Household:", Total.Households, "<br>",
                       "Mean Income: ", Mean.Income, "<br>",
                       "Occupation Score: ", Occupation.Score, "<br>",
                       "Education Score: ", Education.Score)
      )
  })
  
  # Render the interactive bar chart using plotly
  output$barChart <- renderPlotly({
    p <- ggplot(SGdata, aes_string(x = "Planning.Area.of.Residence", y = input$variable)) +
      geom_bar(stat = "identity", fill = "#7F948F", alpha = 0.7) +
      labs(title = paste(input$variable, "by Planning Area"),
           x = "Planning Area",
           y = input$variable) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p)
  })
  
  # Filtered data based on selected area
  filteredData <- reactive({
    if (!is.null(input$areaFilter) && input$areaFilter != "") {
      SGdata[SGdata$Planning.Area.of.Residence == input$areaFilter, ]
    } else {
      SGdata
    }
  })
  
  # Display values for selected area
  output$areaValues <- renderTable({
    filtered <- filteredData()
    if (!is.null(filtered)) {
      data.frame(variable = names(filtered),
                 value = unlist(filtered))
    }
  })
}

shinyApp(ui, server)