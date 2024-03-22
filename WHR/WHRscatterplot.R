pacman::p_load(skimr,tidyverse, patchwork, readr, dplyr, plotly, ggridges, ggiraph, ggthemes,corrplot,GGally, gridExtra)

WHRdata = read.csv ("WHR/World Happiness Data (2015-2019)_cleaned.csv")

scp1 = ggplot(WHRdata, aes(x = GDP.per.Capita, y = Happiness.Score)) + 
  geom_point(size = .5, alpha = 0.8) +  
  geom_smooth(method = "lm", fullrange = TRUE) +
  theme_bw() + labs(title = "Scatter plot with regression line")

scp1a= ggplot(WHRdata, aes(x = GDP.per.Capita, y = Happiness.Score)) + 
  geom_point(aes(color= Region), size = .5, alpha = 0.8) +  
  geom_smooth(aes(color = Region, fill = Region), 
              method = "lm", fullrange = TRUE) +
  facet_wrap(~Region) +
  theme_bw() + labs(title = "Scatter plot with regression line")


scp1|scp1a

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

scp2|scp2a

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

scp3|scp3a

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

scp4|scp4a

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


scp5|scp5a

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


scp6|scp6a

