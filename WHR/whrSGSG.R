pacman::p_load(tidyverse, patchwork, readr, dplyr, plotly, ggplot2, 
               ggHoriPlot, ggthemes, maps , tmap, sf)

mpsz <- st_read(dsn = "C:/AlexeiJason/ISSS608-VAA/WHR/data/geospatial", 
                layer = "MP14_SUBZONE_WEB_PL")

SGDATA = read.csv("C:/AlexeiJason/ISSS608-VAA/WHR/SingaporeIncome.csv")
SGdata_long <- pivot_longer(SGDATA, cols = -Region, names_to = "Variable", values_to = "Value")

library(reshape2)
melted_data <- melt(SGdata_long, id.vars = "Region")

# Plotting
ggplot(melted_data, aes(x = Region, y = value)) +
  geom_bar(stat = "identity") +
  facet_wrap(~variable, scales = "free_y", nrow = 2) +
  labs(title = "Variables by Region",
       x = "Region",
       y = "Value") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))