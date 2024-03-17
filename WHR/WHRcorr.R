library(dplyr)
library(corrplot)
library(ggcorrplot)
library(readr)

WHRdata = read.csv("C:/AlexeiJason/ISSS608-VAA/WHR/World Happiness Data (2015-2019)_cleaned.csv")

dataset = select(WHRdata,-c("Year","Country","Region","Happiness.Rank"))
head(dataset)

Num.cols <- sapply(dataset, is.numeric)
Cor.data <- cor(dataset[, Num.cols])

corrplot(Cor.data, method = 'color') 


Num.cols <- sapply(dataset, is.numeric)


Cor.data <- cor(dataset[, Num.cols])

corrplot_plot <- corrplot(Cor.data, method = 'color', addCoef.col="black", tl.col="black")

cp2 <- ggcorr(dataset, label = TRUE, label_round = 2, label_size = 3.5, size = 2, hjust = .85) +
  ggtitle("Correlation Heatmap") +
  theme(plot.title = element_text(hjust = 0.5))

