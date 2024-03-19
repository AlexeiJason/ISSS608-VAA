library(dplyr)
library(corrplot)
library(ggcorrplot)
library(readr)
library(tidyr)
library(ggplot2)
library(scales)

SGIncome = read.csv("C:/AlexeiJason/ISSS608-VAA/WHR/data/SGIncome.csv")
data_long <- pivot_longer(SGIncome, cols = -Region, names_to = "Income_Level", values_to = "Value")

#Make Income Level in Nice Order

income_levels <- unique(data_long$Income_Level)
income_levels <- income_levels[order(match(income_levels, data_long$Income_Level))]
data_long$Income_Level <- factor(data_long$Income_Level, levels = income_levels)
#Boxplot
ggplot(data_long, aes(x = Income_Level, y = Value, fill = Income_Level)) +
  geom_boxplot() +
  labs(title = "Boxplot of Singapore Income Level", x = "Income Level", y = "Value", fill = "Income Level") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

#Stacked BarChart
ggplot(data_long, aes(x = Region, y = Value, fill = Income_Level)) +
  geom_bar(stat = "identity") +
  labs(title = "Stacked Bar Chart of Singapore Income Level by Region", x = "Region", y = "Value") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  scale_y_continuous(labels = comma)

write_csv(mpsz, path = "MPSZ.csv")
