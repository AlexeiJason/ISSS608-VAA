library(tidyr)
library(shiny)
library(dplyr)
library(MASS)
library(corrplot)
library(ggplot2)


# Read data from year 2010 to 2015
data <- read.csv("c:/AlexeiJason/WHRshiny/data/World Happiness Data (2015-2019)_cleaned.csv")

ui <- fluidPage(
  titlePanel("Happiness Analysis"),
  
  # Create tabs
  tabsetPanel(
    # Tab 1: Happiness Plot
    tabPanel(
      title = "Happiness Score Comparison",
      
      sidebarLayout(
        sidebarPanel(
          selectInput("countries", "Select Countries:", choices = sort(unique(data$Country)), multiple = TRUE, width = "300px"),
          actionButton("generatePlot", "Generate Plot")
        ),
        
        mainPanel(
          plotOutput("happinessPlot")
        )
      )
    ),
    
    # Tab 2: Correlation Matrix
    tabPanel(
      title = "Correlation Matrix",
      sidebarPanel(
        selectInput("country_correlation", "Select Country", choices = sort(unique(data$Country)))
      ),
      mainPanel(
        plotOutput("correlationPlot")
      )
    ),
    
    # Tab 3: Multiple Linear Regression
    tabPanel(
      title = "Multiple Linear Regression",
      sidebarPanel(
        selectInput("scope", "Select Scope", choices = c("World", "Region", "Country")),
        conditionalPanel(
          condition = "input.scope == 'Region'",
          selectInput("region_regression", "Select Region", choices = sort(unique(data$Region)))
        ),
        conditionalPanel(
          condition = "input.scope == 'Country'",
          selectInput("country_regression", "Select Country", choices = sort(unique(data$Country)))
        )
      ),
      mainPanel(
        verbatimTextOutput("regressionSummary")
      )
    ),
    
    # Tab 4: Happiness Score Prediction
    tabPanel(
      title = "Happiness Score Prediction",
      sidebarPanel(
        selectInput("country", "Select Country", choices = sort(unique(data$Country))),
        actionButton("submit", "Calculate Predictions")
      ),
      mainPanel(
        tableOutput("predictions_table")
      )
    )
  )
)




# Define server
server <- function(input, output) {
  
  
  # Generate the plot based on user input
  observeEvent(input$generatePlot, {
    selected_countries <- input$countries
    
    if (length(selected_countries) > 0 && length(selected_countries) <= 5) {
      filtered_data <- filter(data, Country %in% selected_countries)
      
      p <- ggplot(filtered_data, aes(x = Year, y = Happiness.Score, color = Country)) +
        geom_line(size = 2) +
        labs(title = "Happiness Score by Year",
             x = "Year",
             y = "Happiness Score") +
        scale_x_continuous(breaks = c(2015, 2016, 2017, 2018, 2019),
                           labels = c("2015", "2016", "2017", "2018", "2019")) +
        theme_minimal()
      
      output$happinessPlot <- renderPlot({
        p
      })
    }
  })
  
  # Subset data based on selected country
  selected_data <- reactive({
    subset(data, Country == input$country_correlation)
  })
  
  # Prepare correlation matrix
  my_data_continuous <- reactive({
    if (input$scope == "World") {
      world_data <- subset(data, select = -c(Region))
      world_data %>%
        dplyr::select(Year, Happiness.Score, GDP.per.Capita, Social.Support, Healthy.Life.Expectancy, Freedom, Generosity, Perceptions.of.Corruption)
    } else {
      selected_data() %>%
        dplyr::select(Year, Happiness.Score, GDP.per.Capita, Social.Support, Healthy.Life.Expectancy, Freedom, Generosity, Perceptions.of.Corruption)
    }
  })
  
  # Generate correlation plot
  output$correlationPlot <- renderPlot({
    correlation_matrix <- cor(my_data_continuous())
    corrplot(correlation_matrix, method = "color", type = "upper", order = "hclust",
             tl.cex = 0.6, tl.col = "black", tl.srt = 45, addCoef.col = "black")
  })
  
  # Perform multiple linear regression and display summary
  output$regressionSummary <- renderPrint({
    if (input$scope == "World") {
      world_data <- subset(data, select = -c(Region))
      fit <- lm(Happiness.Score ~ Country + Year + GDP.per.Capita + Social.Support + Healthy.Life.Expectancy + Freedom + Generosity + Perceptions.of.Corruption , data = world_data)
    } else if (input$scope == "Region") {
      region_data <- subset(data, Region == input$region_regression)
      fit <- lm(Happiness.Score ~ Country + Year + GDP.per.Capita + Social.Support + Healthy.Life.Expectancy + Freedom + Generosity + Perceptions.of.Corruption, data = region_data)
    } else if (input$scope == "Country") {
      country_data <- subset(data, Country == input$country_regression)
      fit <- lm(Happiness.Score ~ GDP.per.Capita + Healthy.Life.Expectancy + Freedom , data = country_data)
    }
    
    summary(fit)
    step_fit <- stepAIC(fit, direction = "both")
    summary(step_fit)
  })
  
  # Compute predictions for the selected country
  predictions <- reactiveVal()
  
  observeEvent(input$submit, {
    country <- input$country
    
    # Read data from year 2010 to 2015
    data <- read.csv("/Users/tohjanessa/Documents/MITB/ISSS616 Applied Statistics with R/Project/Happiness Project/World Happiness Data (2015-2019)_cleaned.csv")
    
    # Perform multiple linear regression
    #All variables with years
    fit <- lm(Happiness.Score ~ Country + Year + GDP.per.Capita + Healthy.Life.Expectancy + Freedom + Generosity + Perceptions.of.Corruption,
              data = data)
    
    # Perform stepwise regression using stepAIC
    step_fit <- stepAIC(fit, direction = "both")
    summary(step_fit)
    
    # Extract the coefficients from the stepwise regression and replace NA coefficients with 0
    intercept_coef <- ifelse(is.na(coef(step_fit)["(Intercept)"]), 0, coef(step_fit)["(Intercept)"])
    year_coef <- ifelse(is.na(coef(step_fit)["Year"]), 0, coef(step_fit)["Year"])
    gdp_coef <- ifelse(is.na(coef(step_fit)["GDP.per.Capita"]), 0, coef(step_fit)["GDP.per.Capita"])
    social_support_coef <- ifelse(is.na(coef(step_fit)["Social.Support"]), 0, coef(step_fit)["Social.Support"])
    life_expectancy_coef <- ifelse(is.na(coef(step_fit)["Healthy.Life.Expectancy"]), 0, coef(step_fit)["Healthy.Life.Expectancy"])
    freedom_coef <- ifelse(is.na(coef(step_fit)["Freedom"]), 0, coef(step_fit)["Freedom"])
    generosity_coef <- ifelse(is.na(coef(step_fit)["Generosity"]), 0, coef(step_fit)["Generosity"])
    corruption_coef <- ifelse(is.na(coef(step_fit)["Perceptions.of.Corruption"]), 0, coef(step_fit)["Perceptions.of.Corruption"])
    
    
    #Coefficients for different countries
    country_coef <- switch(input$country,
                           "Albania"= Albania_coef <- ifelse(is.na(coef(step_fit)["(Albania)"]), 0, coef(step_fit)["(Albania)"]),
                           "Algeria"= Algeria_coef <- ifelse(is.na(coef(step_fit)["(Algeria)"]), 0, coef(step_fit)["(Algeria)"]),
                           "Argentina"= Argentina_coef <- ifelse(is.na(coef(step_fit)["(Argentina)"]), 0, coef(step_fit)["(Argentina)"]),
                           "Armenia"= Armenia_coef <- ifelse(is.na(coef(step_fit)["(Armenia)"]), 0, coef(step_fit)["(Armenia)"]),
                           "Australia"= Australia_coef <- ifelse(is.na(coef(step_fit)["(Australia)"]), 0, coef(step_fit)["(Australia)"]),
                           "Austria"= Austria_coef <- ifelse(is.na(coef(step_fit)["(Austria)"]), 0, coef(step_fit)["(Austria)"]),
                           "Azerbaijan"= Azerbaijan_coef <- ifelse(is.na(coef(step_fit)["(Azerbaijan)"]), 0, coef(step_fit)["(Azerbaijan)"]),
                           "Bahrain"= Bahrain_coef <- ifelse(is.na(coef(step_fit)["(Bahrain)"]), 0, coef(step_fit)["(Bahrain)"]),
                           "Bangladesh"= Bangladesh_coef <- ifelse(is.na(coef(step_fit)["(Bangladesh)"]), 0, coef(step_fit)["(Bangladesh)"]),
                           "Belarus"= Belarus_coef <- ifelse(is.na(coef(step_fit)["(Belarus)"]), 0, coef(step_fit)["(Belarus)"]),
                           "Belgium"= Belgium_coef <- ifelse(is.na(coef(step_fit)["(Belgium)"]), 0, coef(step_fit)["(Belgium)"]),
                           "Benin"= Benin_coef <- ifelse(is.na(coef(step_fit)["(Benin)"]), 0, coef(step_fit)["(Benin)"]),
                           "Bhutan"= Bhutan_coef <- ifelse(is.na(coef(step_fit)["(Bhutan)"]), 0, coef(step_fit)["(Bhutan)"]),
                           "Bolivia"= Bolivia_coef <- ifelse(is.na(coef(step_fit)["(Bolivia)"]), 0, coef(step_fit)["(Bolivia)"]),
                           "Bosnia and Herzegovina"= Bosnia_and_Herzegovina_coef <- ifelse(is.na(coef(step_fit)["(Bosnia and Herzegovina)"]), 0, coef(step_fit)["(Bosnia and Herzegovina)"]),
                           "Botswana"= Botswana_coef <- ifelse(is.na(coef(step_fit)["(Botswana)"]), 0, coef(step_fit)["(Botswana)"]),
                           "Brazil"= Brazil_coef <- ifelse(is.na(coef(step_fit)["(Brazil)"]), 0, coef(step_fit)["(Brazil)"]),
                           "Bulgaria"= Bulgaria_coef <- ifelse(is.na(coef(step_fit)["(Bulgaria)"]), 0, coef(step_fit)["(Bulgaria)"]),
                           "Burkina Faso"= Burkina_Faso_coef <- ifelse(is.na(coef(step_fit)["(Burkina Faso)"]), 0, coef(step_fit)["(Burkina Faso)"]),
                           "Burundi"= Burundi_coef <- ifelse(is.na(coef(step_fit)["(Burundi)"]), 0, coef(step_fit)["(Burundi)"]),
                           "Cambodia"= Cambodia_coef <- ifelse(is.na(coef(step_fit)["(Cambodia)"]), 0, coef(step_fit)["(Cambodia)"]),
                           "Cameroon"= Cameroon_coef <- ifelse(is.na(coef(step_fit)["(Cameroon)"]), 0, coef(step_fit)["(Cameroon)"]),
                           "Canada"= Canada_coef <- ifelse(is.na(coef(step_fit)["(Canada)"]), 0, coef(step_fit)["(Canada)"]),
                           "Chad"= Chad_coef <- ifelse(is.na(coef(step_fit)["(Chad)"]), 0, coef(step_fit)["(Chad)"]),
                           "Chile"= Chile_coef <- ifelse(is.na(coef(step_fit)["(Chile)"]), 0, coef(step_fit)["(Chile)"]),
                           "China"= China_coef <- ifelse(is.na(coef(step_fit)["(China)"]), 0, coef(step_fit)["(China)"]),
                           "Colombia"= Colombia_coef <- ifelse(is.na(coef(step_fit)["(Colombia)"]), 0, coef(step_fit)["(Colombia)"]),
                           "Congo (Brazzaville)"= Congo_Brazzaville_coef <- ifelse(is.na(coef(step_fit)["(Congo (Brazzaville))"]), 0, coef(step_fit)["(Congo (Brazzaville))"]),
                           "Congo (Kinshasa)"= Congo_Kinshasa_coef <- ifelse(is.na(coef(step_fit)["(Congo (Kinshasa))"]), 0, coef(step_fit)["(Congo (Kinshasa))"]),
                           "Costa Rica"= Costa_Rica_coef <- ifelse(is.na(coef(step_fit)["(Costa Rica)"]), 0, coef(step_fit)["(Costa Rica)"]),
                           "Croatia"= Croatia_coef <- ifelse(is.na(coef(step_fit)["(Croatia)"]), 0, coef(step_fit)["(Croatia)"]),
                           "Cyprus"= Cyprus_coef <- ifelse(is.na(coef(step_fit)["(Cyprus)"]), 0, coef(step_fit)["(Cyprus)"]),
                           "Czech Republic"= Czech_Republic_coef <- ifelse(is.na(coef(step_fit)["(Czech Republic)"]), 0, coef(step_fit)["(Czech Republic)"]),
                           "Denmark"= Denmark_coef <- ifelse(is.na(coef(step_fit)["(Denmark)"]), 0, coef(step_fit)["(Denmark)"]),
                           "Dominican Republic"= Dominican_Republic_coef <- ifelse(is.na(coef(step_fit)["(Dominican Republic)"]), 0, coef(step_fit)["(Dominican Republic)"]),
                           "Ecuador"= Ecuador_coef <- ifelse(is.na(coef(step_fit)["(Ecuador)"]), 0, coef(step_fit)["(Ecuador)"]),
                           "Egypt"= Egypt_coef <- ifelse(is.na(coef(step_fit)["(Egypt)"]), 0, coef(step_fit)["(Egypt)"]),
                           "El Salvador"= El_Salvador_coef <- ifelse(is.na(coef(step_fit)["(El Salvador)"]), 0, coef(step_fit)["(El Salvador)"]),
                           "Estonia"= Estonia_coef <- ifelse(is.na(coef(step_fit)["(Estonia)"]), 0, coef(step_fit)["(Estonia)"]),
                           "Ethiopia"= Ethiopia_coef <- ifelse(is.na(coef(step_fit)["(Ethiopia)"]), 0, coef(step_fit)["(Ethiopia)"]),
                           "Finland"= Finland_coef <- ifelse(is.na(coef(step_fit)["(Finland)"]), 0, coef(step_fit)["(Finland)"]),
                           "France"= France_coef <- ifelse(is.na(coef(step_fit)["(France)"]), 0, coef(step_fit)["(France)"]),
                           "Gabon"= Gabon_coef <- ifelse(is.na(coef(step_fit)["(Gabon)"]), 0, coef(step_fit)["(Gabon)"]),
                           "Georgia"= Georgia_coef <- ifelse(is.na(coef(step_fit)["(Georgia)"]), 0, coef(step_fit)["(Georgia)"]),
                           "Germany"= Germany_coef <- ifelse(is.na(coef(step_fit)["(Germany)"]), 0, coef(step_fit)["(Germany)"]),
                           "Ghana"= Ghana_coef <- ifelse(is.na(coef(step_fit)["(Ghana)"]), 0, coef(step_fit)["(Ghana)"]),
                           "Greece"= Greece_coef <- ifelse(is.na(coef(step_fit)["(Greece)"]), 0, coef(step_fit)["(Greece)"]),
                           "Guatemala"= Guatemala_coef <- ifelse(is.na(coef(step_fit)["(Guatemala)"]), 0, coef(step_fit)["(Guatemala)"]),
                           "Guinea"= Guinea_coef <- ifelse(is.na(coef(step_fit)["(Guinea)"]), 0, coef(step_fit)["(Guinea)"]),
                           "Haiti"= Haiti_coef <- ifelse(is.na(coef(step_fit)["(Haiti)"]), 0, coef(step_fit)["(Haiti)"]),
                           "Honduras"= Honduras_coef <- ifelse(is.na(coef(step_fit)["(Honduras)"]), 0, coef(step_fit)["(Honduras)"]),
                           "Hungary"= Hungary_coef <- ifelse(is.na(coef(step_fit)["(Hungary)"]), 0, coef(step_fit)["(Hungary)"]),
                           "Iceland"= Iceland_coef <- ifelse(is.na(coef(step_fit)["(Iceland)"]), 0, coef(step_fit)["(Iceland)"]),
                           "India"= India_coef <- ifelse(is.na(coef(step_fit)["(India)"]), 0, coef(step_fit)["(India)"]),
                           "Indonesia"= Indonesia_coef <- ifelse(is.na(coef(step_fit)["(Indonesia)"]), 0, coef(step_fit)["(Indonesia)"]),
                           "Iran"= Iran_coef <- ifelse(is.na(coef(step_fit)["(Iran)"]), 0, coef(step_fit)["(Iran)"]),
                           "Iraq"= Iraq_coef <- ifelse(is.na(coef(step_fit)["(Iraq)"]), 0, coef(step_fit)["(Iraq)"]),
                           "Ireland"= Ireland_coef <- ifelse(is.na(coef(step_fit)["(Ireland)"]), 0, coef(step_fit)["(Ireland)"]),
                           "Israel"= Israel_coef <- ifelse(is.na(coef(step_fit)["(Israel)"]), 0, coef(step_fit)["(Israel)"]),
                           "Italy"= Italy_coef <- ifelse(is.na(coef(step_fit)["(Italy)"]), 0, coef(step_fit)["(Italy)"]),
                           "Ivory Coast"= Ivory_Coast_coef <- ifelse(is.na(coef(step_fit)["(Ivory Coast)"]), 0, coef(step_fit)["(Ivory Coast)"]),
                           "Jamaica"= Jamaica_coef <- ifelse(is.na(coef(step_fit)["(Jamaica)"]), 0, coef(step_fit)["(Jamaica)"]),
                           "Japan"= Japan_coef <- ifelse(is.na(coef(step_fit)["(Japan)"]), 0, coef(step_fit)["(Japan)"]),
                           "Jordan"= Jordan_coef <- ifelse(is.na(coef(step_fit)["(Jordan)"]), 0, coef(step_fit)["(Jordan)"]),
                           "Kazakhstan"= Kazakhstan_coef <- ifelse(is.na(coef(step_fit)["(Kazakhstan)"]), 0, coef(step_fit)["(Kazakhstan)"]),
                           "Kenya"= Kenya_coef <- ifelse(is.na(coef(step_fit)["(Kenya)"]), 0, coef(step_fit)["(Kenya)"]),
                           "Kosovo"= Kosovo_coef <- ifelse(is.na(coef(step_fit)["(Kosovo)"]), 0, coef(step_fit)["(Kosovo)"]),
                           "Kuwait"= Kuwait_coef <- ifelse(is.na(coef(step_fit)["(Kuwait)"]), 0, coef(step_fit)["(Kuwait)"]),
                           "Kyrgyzstan"= Kyrgyzstan_coef <- ifelse(is.na(coef(step_fit)["(Kyrgyzstan)"]), 0, coef(step_fit)["(Kyrgyzstan)"]),
                           "Latvia"= Latvia_coef <- ifelse(is.na(coef(step_fit)["(Latvia)"]), 0, coef(step_fit)["(Latvia)"]),
                           "Lebanon"= Lebanon_coef <- ifelse(is.na(coef(step_fit)["(Lebanon)"]), 0, coef(step_fit)["(Lebanon)"]),
                           "Liberia"= Liberia_coef <- ifelse(is.na(coef(step_fit)["(Liberia)"]), 0, coef(step_fit)["(Liberia)"]),
                           "Libya"= Libya_coef <- ifelse(is.na(coef(step_fit)["(Libya)"]), 0, coef(step_fit)["(Libya)"]),
                           "Lithuania"= Lithuania_coef <- ifelse(is.na(coef(step_fit)["(Lithuania)"]), 0, coef(step_fit)["(Lithuania)"]),
                           "Luxembourg"= Luxembourg_coef <- ifelse(is.na(coef(step_fit)["(Luxembourg)"]), 0, coef(step_fit)["(Luxembourg)"]),
                           "Madagascar"= Madagascar_coef <- ifelse(is.na(coef(step_fit)["(Madagascar)"]), 0, coef(step_fit)["(Madagascar)"]),
                           "Malawi"= Malawi_coef <- ifelse(is.na(coef(step_fit)["(Malawi)"]), 0, coef(step_fit)["(Malawi)"]),
                           "Malaysia"= Malaysia_coef <- ifelse(is.na(coef(step_fit)["(Malaysia)"]), 0, coef(step_fit)["(Malaysia)"]),
                           "Mali"= Mali_coef <- ifelse(is.na(coef(step_fit)["(Mali)"]), 0, coef(step_fit)["(Mali)"]),
                           "Malta"= Malta_coef <- ifelse(is.na(coef(step_fit)["(Malta)"]), 0, coef(step_fit)["(Malta)"]),
                           "Mauritania"= Mauritania_coef <- ifelse(is.na(coef(step_fit)["(Mauritania)"]), 0, coef(step_fit)["(Mauritania)"]),
                           "Mauritius"= Mauritius_coef <- ifelse(is.na(coef(step_fit)["(Mauritius)"]), 0, coef(step_fit)["(Mauritius)"]),
                           "Mexico"= Mexico_coef <- ifelse(is.na(coef(step_fit)["(Mexico)"]), 0, coef(step_fit)["(Mexico)"]),
                           "Moldova"= Moldova_coef <- ifelse(is.na(coef(step_fit)["(Moldova)"]), 0, coef(step_fit)["(Moldova)"]),
                           "Mongolia"= Mongolia_coef <- ifelse(is.na(coef(step_fit)["(Mongolia)"]), 0, coef(step_fit)["(Mongolia)"]),
                           "Montenegro"= Montenegro_coef <- ifelse(is.na(coef(step_fit)["(Montenegro)"]), 0, coef(step_fit)["(Montenegro)"]),
                           "Morocco"= Morocco_coef <- ifelse(is.na(coef(step_fit)["(Morocco)"]), 0, coef(step_fit)["(Morocco)"]),
                           "Myanmar"= Myanmar_coef <- ifelse(is.na(coef(step_fit)["(Myanmar)"]), 0, coef(step_fit)["(Myanmar)"]),
                           "Nepal"= Nepal_coef <- ifelse(is.na(coef(step_fit)["(Nepal)"]), 0, coef(step_fit)["(Nepal)"]),
                           "Netherlands"= Netherlands_coef <- ifelse(is.na(coef(step_fit)["(Netherlands)"]), 0, coef(step_fit)["(Netherlands)"]),
                           "New Zealand"= New_Zealand_coef <- ifelse(is.na(coef(step_fit)["(New Zealand)"]), 0, coef(step_fit)["(New Zealand)"]),
                           "Nicaragua"= Nicaragua_coef <- ifelse(is.na(coef(step_fit)["(Nicaragua)"]), 0, coef(step_fit)["(Nicaragua)"]),
                           "Niger"= Niger_coef <- ifelse(is.na(coef(step_fit)["(Niger)"]), 0, coef(step_fit)["(Niger)"]),
                           "Nigeria"= Nigeria_coef <- ifelse(is.na(coef(step_fit)["(Nigeria)"]), 0, coef(step_fit)["(Nigeria)"]),
                           "Norway"= Norway_coef <- ifelse(is.na(coef(step_fit)["(Norway)"]), 0, coef(step_fit)["(Norway)"]),
                           "Pakistan"= Pakistan_coef <- ifelse(is.na(coef(step_fit)["(Pakistan)"]), 0, coef(step_fit)["(Pakistan)"]),
                           "Palestinian Territories"= Palestinian_Territories_coef <- ifelse(is.na(coef(step_fit)["(Palestinian Territories)"]), 0, coef(step_fit)["(Palestinian Territories)"]),
                           "Panama"= Panama_coef <- ifelse(is.na(coef(step_fit)["(Panama)"]), 0, coef(step_fit)["(Panama)"]),
                           "Paraguay"= Paraguay_coef <- ifelse(is.na(coef(step_fit)["(Paraguay)"]), 0, coef(step_fit)["(Paraguay)"]),
                           "Peru"= Peru_coef <- ifelse(is.na(coef(step_fit)["(Peru)"]), 0, coef(step_fit)["(Peru)"]),
                           "Philippines"= Philippines_coef <- ifelse(is.na(coef(step_fit)["(Philippines)"]), 0, coef(step_fit)["(Philippines)"]),
                           "Poland"= Poland_coef <- ifelse(is.na(coef(step_fit)["(Poland)"]), 0, coef(step_fit)["(Poland)"]),
                           "Portugal"= Portugal_coef <- ifelse(is.na(coef(step_fit)["(Portugal)"]), 0, coef(step_fit)["(Portugal)"]),
                           "Qatar"= Qatar_coef <- ifelse(is.na(coef(step_fit)["(Qatar)"]), 0, coef(step_fit)["(Qatar)"]),
                           "Romania"= Romania_coef <- ifelse(is.na(coef(step_fit)["(Romania)"]), 0, coef(step_fit)["(Romania)"]),
                           "Russia"= Russia_coef <- ifelse(is.na(coef(step_fit)["(Russia)"]), 0, coef(step_fit)["(Russia)"]),
                           "Rwanda"= Rwanda_coef <- ifelse(is.na(coef(step_fit)["(Rwanda)"]), 0, coef(step_fit)["(Rwanda)"]),
                           "Saudi Arabia"= Saudi_Arabia_coef <- ifelse(is.na(coef(step_fit)["(Saudi Arabia)"]), 0, coef(step_fit)["(Saudi Arabia)"]),
                           "Senegal"= Senegal_coef <- ifelse(is.na(coef(step_fit)["(Senegal)"]), 0, coef(step_fit)["(Senegal)"]),
                           "Serbia"= Serbia_coef <- ifelse(is.na(coef(step_fit)["(Serbia)"]), 0, coef(step_fit)["(Serbia)"]),
                           "Sierra Leone"= Sierra_Leone_coef <- ifelse(is.na(coef(step_fit)["(Sierra Leone)"]), 0, coef(step_fit)["(Sierra Leone)"]),
                           "Singapore"= Singapore_coef <- ifelse(is.na(coef(step_fit)["(Singapore)"]), 0, coef(step_fit)["(Singapore)"]),
                           "Slovakia"= Slovakia_coef <- ifelse(is.na(coef(step_fit)["(Slovakia)"]), 0, coef(step_fit)["(Slovakia)"]),
                           "Slovenia"= Slovenia_coef <- ifelse(is.na(coef(step_fit)["(Slovenia)"]), 0, coef(step_fit)["(Slovenia)"]),
                           "South Africa"= South_Africa_coef <- ifelse(is.na(coef(step_fit)["(South Africa)"]), 0, coef(step_fit)["(South Africa)"]),
                           "South Korea"= South_Korea_coef <- ifelse(is.na(coef(step_fit)["(South Korea)"]), 0, coef(step_fit)["(South Korea)"]),
                           "Spain"= Spain_coef <- ifelse(is.na(coef(step_fit)["(Spain)"]), 0, coef(step_fit)["(Spain)"]),
                           "Sri Lanka"= Sri_Lanka_coef <- ifelse(is.na(coef(step_fit)["(Sri Lanka)"]), 0, coef(step_fit)["(Sri Lanka)"]),
                           "Sweden"= Sweden_coef <- ifelse(is.na(coef(step_fit)["(Sweden)"]), 0, coef(step_fit)["(Sweden)"]),
                           "Switzerland"= Switzerland_coef <- ifelse(is.na(coef(step_fit)["(Switzerland)"]), 0, coef(step_fit)["(Switzerland)"]),
                           "Syria"= Syria_coef <- ifelse(is.na(coef(step_fit)["(Syria)"]), 0, coef(step_fit)["(Syria)"]),
                           "Tajikistan"= Tajikistan_coef <- ifelse(is.na(coef(step_fit)["(Tajikistan)"]), 0, coef(step_fit)["(Tajikistan)"]),
                           "Tanzania"= Tanzania_coef <- ifelse(is.na(coef(step_fit)["(Tanzania)"]), 0, coef(step_fit)["(Tanzania)"]),
                           "Thailand"= Thailand_coef <- ifelse(is.na(coef(step_fit)["(Thailand)"]), 0, coef(step_fit)["(Thailand)"]),
                           "Togo"= Togo_coef <- ifelse(is.na(coef(step_fit)["(Togo)"]), 0, coef(step_fit)["(Togo)"]),
                           "Tunisia"= Tunisia_coef <- ifelse(is.na(coef(step_fit)["(Tunisia)"]), 0, coef(step_fit)["(Tunisia)"]),
                           "Turkey"= Turkey_coef <- ifelse(is.na(coef(step_fit)["(Turkey)"]), 0, coef(step_fit)["(Turkey)"]),
                           "Turkmenistan"= Turkmenistan_coef <- ifelse(is.na(coef(step_fit)["(Turkmenistan)"]), 0, coef(step_fit)["(Turkmenistan)"]),
                           "Uganda"= Uganda_coef <- ifelse(is.na(coef(step_fit)["(Uganda)"]), 0, coef(step_fit)["(Uganda)"]),
                           "Ukraine"= Ukraine_coef <- ifelse(is.na(coef(step_fit)["(Ukraine)"]), 0, coef(step_fit)["(Ukraine)"]),
                           "United Arab Emirates"= United_Arab_Emirates_coef <- ifelse(is.na(coef(step_fit)["(United Arab Emirates)"]), 0, coef(step_fit)["(United Arab Emirates)"]),
                           "United Kingdom"= United_Kingdom_coef <- ifelse(is.na(coef(step_fit)["(United Kingdom)"]), 0, coef(step_fit)["(United Kingdom)"]),
                           "United States"= United_States_coef <- ifelse(is.na(coef(step_fit)["(United States)"]), 0, coef(step_fit)["(United States)"]),
                           "Uruguay"= Uruguay_coef <- ifelse(is.na(coef(step_fit)["(Uruguay)"]), 0, coef(step_fit)["(Uruguay)"]),
                           "Uzbekistan"= Uzbekistan_coef <- ifelse(is.na(coef(step_fit)["(Uzbekistan)"]), 0, coef(step_fit)["(Uzbekistan)"]),
                           "Venezuela"= Venezuela_coef <- ifelse(is.na(coef(step_fit)["(Venezuela)"]), 0, coef(step_fit)["(Venezuela)"]),
                           "Vietnam"= Vietnam_coef <- ifelse(is.na(coef(step_fit)["(Vietnam)"]), 0, coef(step_fit)["(Vietnam)"]),
                           "Yemen"= Yemen_coef <- ifelse(is.na(coef(step_fit)["(Yemen)"]), 0, coef(step_fit)["(Yemen)"]),
                           "Zambia"= Zambia_coef <- ifelse(is.na(coef(step_fit)["(Zambia)"]), 0, coef(step_fit)["(Zambia)"]),
                           "Zimbabwe"= Zimbabwe_coef <- ifelse(is.na(coef(step_fit)["(Zimbabwe)"]), 0, coef(step_fit)["(Zimbabwe)"])
    ) 
    
    
    # Create a vector of years from 2020 to 2050
    years <- 2020:2050
    
    #Filter data by selected country
    country_data <- subset(data, Country == country)
    
    # Calculate the predicted Happiness.Score for each year
    predicted_scores <- intercept_coef +
      year_coef * years +
      gdp_coef * country_data$GDP.per.Capita +
      life_expectancy_coef * country_data$Healthy.Life.Expectancy + 
      freedom_coef * country_data$Freedom +
      generosity_coef * country_data$Generosity +
      corruption_coef * country_data$Perceptions.of.Corruption + country_coef 
    
    # Create a data frame with the predictions
    predictions_df <- data.frame(
      Country = rep(country, length(years)),
      Year = years,
      Happiness.Score = predicted_scores
    )
    
    predictions(predictions_df)
  })
  
  # Display the predictions table
  output$predictions_table <- renderTable({
    predictions()
  })
}

# Run the shiny app
shinyApp(ui = ui, server = server)
