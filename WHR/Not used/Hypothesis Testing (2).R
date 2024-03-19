library(shiny)
library(ggpubr)
library(ggplot2)
library(dplyr)
library(car)

# Define UI
ui <- fluidPage(
  titlePanel("World Happiness Data Analysis"),
  sidebarLayout(
    sidebarPanel(
      helpText("Select the Income Class to display its Happiness Score analysis."),
      selectInput("incomeClass", "Income Class",
                  choices = c("All", "Low", "Lower middle", "Upper middle", "High"),
                  selected = "All")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Quartiles", verbatimTextOutput("quartilesOutput")),
        tabPanel("Box Plot", plotOutput("boxPlot", height = "600px")),
        tabPanel("ANOVA Summary", verbatimTextOutput("anovaSummary")),
        tabPanel("Tukey's HSD", verbatimTextOutput("tukeyHSD")),
        tabPanel("Residuals vs Fits", plotOutput("residualsFits", height = "600px")),
        tabPanel("Levene's Test", verbatimTextOutput("leveneTest")),
        tabPanel("Normality Test", verbatimTextOutput("normalityTest")),
        tabPanel("Kruskal-Wallis Test", verbatimTextOutput("kruskalTest")),
        tabPanel("Pairwise Comparison", verbatimTextOutput("pairwiseTest"))
      )
    )
  )
)
# Define server logic
server <- function(input, output) {
  # Read data
  meandata <- read.csv("data/World Happiness Data (2015-2019)_mean.csv")
  icdata <- read.csv("data/World Happiness Data (2015-2019)_incomeclass.csv")
  
  # Reactively filter data based on the selected income class
  filteredData <- reactive({
    if(input$incomeClass == "All") {
      icdata
    } else {
      icdata %>% filter(Income.Class == input$incomeClass)
    }
  })
  
  # Calculate and display quartiles
  output$quartilesOutput <- renderPrint({
    quantile(filteredData()$Happiness.Score, probs = c(0.25, 0.5, 0.75))
  })
  
  # Generate and display box plot
  output$boxPlot <- renderPlot({
    ggboxplot(filteredData(), x = "Income.Class", y = "Happiness.Score",
              color = "Income.Class", palette = c("#00AFBB", "#E7B800", "#FC4E07", "#0000FF"),
              order = c("Low", "Lower middle", "Upper middle", "High"),
              ylab = "Happiness Score", xlab = "Income Class") + 
      theme(plot.title = element_text(size = 20))
  }, res = 96) # Increase resolution to make plot larger
  
  # ANOVA summary
  output$anovaSummary <- renderPrint({
    res.aov <- aov(Happiness.Score ~ Income.Class, data = filteredData())
    summary(res.aov)
  })
  
  # Tukey's HSD
  output$tukeyHSD <- renderPrint({
    res.aov <- aov(Happiness.Score ~ Income.Class, data = filteredData())
    TukeyHSD(res.aov)
  })
  
  # Residuals vs Fits Plot
  output$residualsFits <- renderPlot({
    res.aov <- aov(Happiness.Score ~ Income.Class, data = filteredData())
    plot(res.aov, 1)
  }, res = 96)
  
  # Levene's Test
  output$leveneTest <- renderPrint({
    leveneTest(Happiness.Score ~ Income.Class, data = filteredData())
  })
  
  # Normality Test
  output$normalityTest <- renderPrint({
    res.aov <- aov(Happiness.Score ~ Income.Class, data = filteredData())
    aov_residuals <- residuals(object = res.aov)
    shapiro.test(x=aov_residuals)
  })
  
  # Kruskal-Wallis Test
  output$kruskalTest <- renderPrint({
    kruskal.test(Happiness.Score ~ Income.Class, data = filteredData())
  })
  
  # Pairwise-comparison Test
  output$pairwiseTest <- renderPrint({
    pairwise.wilcox.test(filteredData()$Happiness.Score, filteredData()$Income.Class, p.adjust.method = "BH")
  })
}

# Run the application
shinyApp(ui = ui, server = server)