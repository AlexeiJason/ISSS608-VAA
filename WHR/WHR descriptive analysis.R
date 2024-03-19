pacman::p_load(tidyverse, dplyr, plotly, ggplot2, ggthemes,reshape2, patchwork)
WHRdata = read.csv ("C:/AlexeiJason/ISSS608-VAA/WHR/World Happiness Data (2015-2019)_cleaned.csv")

#Top 10 Happiness Score Countries for each year


plot_happiest_countries <- function(WHRdata, year) {
  WHRdata %>%
    filter(Year == year) %>%
    arrange(desc(Happiness.Score)) %>%
    slice_head(n = 10) %>%
    ggplot(aes(reorder(Country, Happiness.Score), Happiness.Score)) +
    geom_point(colour = "#459395", size = 3) +
    theme(text = element_text(size = 10)) + 
    coord_flip() +
    labs(title = paste( year), x = "")
}

years <- 2015:2019
plots <- lapply(years, function(year) plot_happiest_countries(WHRdata, year))


plots_arranged <- wrap_plots(plots, ncol = length(years))
plots_arranged


#Mean Happiness Score by Regions
gg2 <- ggplot(WHRdata , aes(x = Region, y = Happiness.Score)) +
  geom_boxplot(aes(fill=Region)) + theme_bw() + labs(
  title = " Mean Happiness Score by Regions from 2015-2019")+
  theme(axis.text.x = element_text (angle = 90))

gg2

#Top 10 Happiness Score Countries
plot_avg_happiness <- WHRdata %>%
  group_by(Country) %>%
  summarise(mscore = mean(Happiness.Score)) %>%
  arrange(-mscore) %>%
  slice_head(n = 10) %>%
  ggplot(aes(reorder(Country, mscore), mscore)) +
  geom_point() +
  theme_bw() +
  coord_flip() +
  labs(title = "Top 10 Happiest Countries",
       x = "", y = "Average Happiness Score")

Top10_happy_country_DF <- WHRdata %>%
  group_by(Country) %>%
  summarise(mscore = mean(Happiness.Score)) %>%
  arrange(-mscore) %>%
  slice_head(n = 10)

Top10_happy_country_DF_list <- Top10_happy_country_DF$Country


WHRdata_Top10_happy_country <- WHRdata %>%
  filter(Country %in% Top10_happy_country_DF_list)

plot_happiness_trend <- ggplot(WHRdata_Top10_happy_country, 
                               aes(x = Year, y = Happiness.Score, color = Country)) +
  geom_line() +
  labs(title = "Trend of Happiness Scores")


plots_side_by_side <- plot_avg_happiness + plot_happiness_trend
plots_side_by_side

#Top 10 Progressive Countries

WHRdata_progress <- WHRdata %>%
  mutate(y = as.character(Year)) %>%
  select(y, Country, Happiness.Score) %>%
  pivot_wider(names_from = y, values_from = Happiness.Score,
              names_prefix = "y_") %>%
  mutate(p = (y_2019 - y_2015) / y_2015 * 100) %>%
  arrange(-p) %>%
  slice_head(n = 10)


scatter_plot <- ggplot(WHRdata_progress, aes(reorder(Country, p), p)) +
  geom_point() +
  theme_bw() +
  coord_flip() +
  labs(title = "The 10 Most Progressive Countries",
       y = "Percentage Increase of Happiness Score", x = "")


WHRdata_Top10_Progress_country <- WHRdata %>%
  filter(Country %in% WHRdata_progress$Country)


line_plot <- ggplot(WHRdata_Top10_Progress_country, aes(x = Year, y = Happiness.Score, color = Country)) +
  geom_line() +
  labs(title = "Trend of Happiness Scores")


side_by_side_plots <- scatter_plot + line_plot
side_by_side_plots

#Mean Value of Happiness Indexes

WHRdata %>%
  summarise(Economy = mean(GDP.per.Capita),
            Family = mean(Social.Support),
            Health = mean(Healthy.Life.Expectancy),
            Freedom = mean(Freedom),
            Generosity = mean(Generosity),
            Trust = mean(Perceptions.of.Corruption)) %>%
  pivot_longer(c(Economy, Family, Health,Freedom,Generosity, Trust),
               names_to = "f", values_to = "value") %>%
  ggplot(aes(reorder(f, value), value)) +
  geom_bar(stat = "identity", fill = "lightblue", width = 0.55, alpha = 0.7) +
  geom_text(aes(label = paste0(round(value, 2)), vjust = -0.5)) +
  theme_bw() +
  labs(title = "The Mean Value of Happiness Indexes" , y = "", x = "")

#Average Value of Happiness Indexes for Different Regions

Happiness.Continent <- WHRdata %>%
  select(-c(Year,Happiness.Rank))%>%
  group_by(Region) %>%
  summarise_at(vars(-Country), funs(mean(., na.rm=TRUE)))


Happiness.Continent.melt <- melt(Happiness.Continent,id.vars = "Region")
ggplot(Happiness.Continent.melt, aes(y=value, x=Region, color=Region, fill=Region)) + 
  geom_bar( stat="identity") +    
  facet_wrap(~variable) + theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Average Value of Happiness Indexes for Different Regions", 
       y = "Average value") 



