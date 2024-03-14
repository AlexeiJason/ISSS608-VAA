pacman::p_load(skimr,tidyverse, patchwork, readr, dplyr, plotly, ggridges, ggiraph, ggthemes,corrplot,GGally, gridExtra)

data = read.csv ("World Happiness Data (2010-2019)_cleaned.csv")

WHRdata <- data %>%
  filter(Year >= 2010 & Year <= 2019)

scp1 = ggplot(WHRdata, aes(x = Log.GDP.per.Capita, y = Happiness.Score)) + 
  geom_point(size = .5, alpha = 0.8) +  
  geom_smooth(method = "lm", fullrange = TRUE) +
  theme_bw() + labs(title = "Scatter plot with regression line")

scp1a= ggplot(WHRdata, aes(x = Log.GDP.per.Capita, y = Happiness.Score)) + 
  geom_point(aes(color= Region), size = .5, alpha = 0.8) +  
  geom_smooth(aes(color = Region, fill = Region), 
              method = "lm", fullrange = TRUE) +
  facet_wrap(~Region) +
  theme_bw() + labs(title = "Scatter plot with regression line")

scp1b= ggplot(WHRdata, aes(x = Log.GDP.per.Capita, y = Happiness.Score)) + 
  geom_point(aes(color= sub.region), size = .5, alpha = 0.8) +  
  geom_smooth(aes(color = sub.region, fill = sub.region), 
              method = "lm", fullrange = TRUE) +
  facet_wrap(~sub.region) +
  theme_bw() + labs(title = "Scatter plot with regression line")

scp1|scp1a|scp1b

scp2 = ggplot(WHRdata, aes(x = Social.Support, y = Happiness.Score)) + 
  geom_point(size = .5, alpha = 0.8) +  
  geom_smooth(method = "lm", fullrange = TRUE) +
  theme_bw() + labs(title = "Scatter plot with regression line")

scp2a= ggplot(WHRdata, aes(x = Social.Support, y = Happiness.Score)) + 
  geom_point(aes(color= Region), size = .5, alpha = 0.8) +  
  geom_smooth(aes(color = Region, fill = Region), 
              method = "lm", fullrange = TRUE) +
  facet_wrap(~Region) +
  theme_bw() + labs(title = "Scatter plot with regression line")

scp2b= ggplot(WHRdata, aes(x = Social.Support, y = Happiness.Score)) + 
  geom_point(aes(color= sub.region), size = .5, alpha = 0.8) +  
  geom_smooth(aes(color = sub.region, fill = sub.region), 
              method = "lm", fullrange = TRUE) +
  facet_wrap(~sub.region) +
  theme_bw() + labs(title = "Scatter plot with regression line")

scp2|scp2a|scp2b

scp3 = ggplot(WHRdata, aes(x = Healthy.Life.Expectancy, y = Happiness.Score)) + 
  geom_point(size = .5, alpha = 0.8) +  
  geom_smooth(method = "lm", fullrange = TRUE) +
  theme_bw() + labs(title = "Scatter plot with regression line")

scp3a= ggplot(WHRdata, aes(x = Healthy.Life.Expectancy, y = Happiness.Score)) + 
  geom_point(aes(color= Region), size = .5, alpha = 0.8) +  
  geom_smooth(aes(color = Region, fill = Region), 
              method = "lm", fullrange = TRUE) +
  facet_wrap(~Region) +
  theme_bw() + labs(title = "Scatter plot with regression line")

scp3b= ggplot(WHRdata, aes(x = Healthy.Life.Expectancy, y = Happiness.Score)) + 
  geom_point(aes(color= sub.region), size = .5, alpha = 0.8) +  
  geom_smooth(aes(color = sub.region, fill = sub.region), 
              method = "lm", fullrange = TRUE) +
  facet_wrap(~sub.region) +
  theme_bw() + labs(title = "Scatter plot with regression line")

scp3|scp3a|scp3b

scp4 = ggplot(WHRdata, aes(x = Freedom, y = Happiness.Score)) + 
  geom_point(size = .5, alpha = 0.8) +  
  geom_smooth(method = "lm", fullrange = TRUE) +
  theme_bw() + labs(title = "Scatter plot with regression line")

scp4a= ggplot(WHRdata, aes(x = Freedom, y = Happiness.Score)) + 
  geom_point(aes(color= Region), size = .5, alpha = 0.8) +  
  geom_smooth(aes(color = Region, fill = Region), 
              method = "lm", fullrange = TRUE) +
  facet_wrap(~Region) +
  theme_bw() + labs(title = "Scatter plot with regression line")

scp4b= ggplot(WHRdata, aes(x = Freedom, y = Happiness.Score)) + 
  geom_point(aes(color= sub.region), size = .5, alpha = 0.8) +  
  geom_smooth(aes(color = sub.region, fill = sub.region), 
              method = "lm", fullrange = TRUE) +
  facet_wrap(~sub.region) +
  theme_bw() + labs(title = "Scatter plot with regression line")

scp4|scp4a|scp4b

scp5 = ggplot(WHRdata, aes(x = Perceptions.of.Corruption, y = Happiness.Score)) + 
  geom_point(size = .5, alpha = 0.8) +  
  geom_smooth(method = "lm", fullrange = TRUE) +
  theme_bw() + labs(title = "Scatter plot with regression line")

scp5a= ggplot(WHRdata, aes(x = Perceptions.of.Corruption, y = Happiness.Score)) + 
  geom_point(aes(color= Region), size = .5, alpha = 0.8) +  
  geom_smooth(aes(color = Region, fill = Region), 
              method = "lm", fullrange = TRUE) +
  facet_wrap(~Region) +
  theme_bw() + labs(title = "Scatter plot with regression line")

scp5b= ggplot(WHRdata, aes(x = Perceptions.of.Corruption, y = Happiness.Score)) + 
  geom_point(aes(color= sub.region), size = .5, alpha = 0.8) +  
  geom_smooth(aes(color = sub.region, fill = sub.region), 
              method = "lm", fullrange = TRUE) +
  facet_wrap(~sub.region) +
  theme_bw() + labs(title = "Scatter plot with regression line")

scp5|scp5a|scp5b

scp6 = ggplot(WHRdata, aes(x = Generosity, y = Happiness.Score)) + 
  geom_point(size = .5, alpha = 0.8) +  
  geom_smooth(method = "lm", fullrange = TRUE) +
  theme_bw() + labs(title = "Scatter plot with regression line")

scp6a= ggplot(WHRdata, aes(x = Generosity, y = Happiness.Score)) + 
  geom_point(aes(color= Region), size = .5, alpha = 0.8) +  
  geom_smooth(aes(color = Region, fill = Region), 
              method = "lm", fullrange = TRUE) +
  facet_wrap(~Region) +
  theme_bw() + labs(title = "Scatter plot with regression line")

scp6b= ggplot(WHRdata, aes(x = Generosity, y = Happiness.Score)) + 
  geom_point(aes(color= sub.region), size = .5, alpha = 0.8) +  
  geom_smooth(aes(color = sub.region, fill = sub.region), 
              method = "lm", fullrange = TRUE) +
  facet_wrap(~sub.region) +
  theme_bw() + labs(title = "Scatter plot with regression line")

scp6|scp6a|scp6b

dataset = select(WHRdata,-c("Year","Country","Region","sub.region"))

Num.cols <- sapply(dataset, is.numeric)


Cor.data <- cor(dataset[, Num.cols])

corrplot_plot <- corrplot(Cor.data, method = 'color', addCoef.col="black", tl.col="black")

cp2 <- ggcorr(dataset, label = TRUE, label_round = 2, label_size = 3.5, size = 2, hjust = .85) +
  ggtitle("Correlation Heatmap") +
  theme(plot.title = element_text(hjust = 0.5))