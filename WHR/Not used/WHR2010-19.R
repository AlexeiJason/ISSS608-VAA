pacman::p_load(skimr,tidyverse, patchwork, readr, dplyr, plotly, ggridges, ggiraph, ggthemes,corrplot,GGally)
data = read.csv ("C:/AlexeiJason/ISSS608-VAA/WHR/World Happiness Data (2010-2019)_cleaned.csv")

WHRdata <- data %>%
  filter(Year >= 2010 & Year <= 2019)

#Input
input_index <- "Log.GDP.per.Capita"

#Log.GDP.per.Capita
#Social.support
#Healthy.Life.Expectancy
#Freedom
#Generosity
#Perception.of.Corruption


scp1 = ggplot(WHRdata, aes(x = input_index, y = Happiness.Score)) + 
  geom_point(size = .5, alpha = 0.8) +  
  geom_smooth(method = "lm", fullrange = TRUE) +
  theme_bw() + labs(title = "Scatter plot with regression line")

scp1a= ggplot(WHRdata, aes(x = input_index, y = Happiness.Score)) + 
  geom_point(aes(color=Region), size = .5, alpha = 0.8) +  
  geom_smooth(aes(color = Region, fill = Region), 
              method = "lm", fullrange = TRUE) +
  facet_wrap(~Region) +
  theme_bw() + labs(title = "Scatter plot with regression line")

scp1b= ggplot(WHRdata, aes(x = input_index, y = Happiness.Score)) + 
  geom_point(aes(color=sub.region), size = .5, alpha = 0.8) +  
  geom_smooth(aes(color = sub.region, fill = sub.region), 
              method = "lm", fullrange = TRUE) +
  facet_wrap(~sub.region) +
  theme_bw() + labs(title = "Scatter plot with regression line")

scp1
scp1a
scp1b