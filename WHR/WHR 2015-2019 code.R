# Data Preparation

## Importing packages


pacman::p_load(skimr,tidyverse, patchwork, readr, dplyr, plotly, ggridges, ggiraph, ggthemes,corrplot,GGally)


## Importing Datasets


WHRdata2015 = read.csv ("data/WHRdata2015.csv")
WHRdata2016 = read.csv ("data/WHRdata2016.csv")
WHRdata2017 = read.csv ("data/WHRdata2017.csv")
WHRdata2018 = read.csv ("data/WHRdata2018.csv")
WHRdata2019 = read.csv ("data/WHRdata2019.csv")
head(WHRdata2019)



WHRdata2019=plyr::rename(WHRdata2019, replace = c( "Country.or.region"="Country", 
                                                   "Overall.rank"="Happiness.Rank" ,
                                                   "GDP.per.capita"="Economy..GDP.per.Capita.",
                                                   "Healthy.life.expectancy"="Health..Life.Expectancy.",
                                                   "Freedom.to.make.life.choices"="Freedom",
                                                   "Perceptions.of.corruption"="Trust..Government.Corruption.",
                                                   "Social.support"="Family",
                                                   "Score"="Happiness.Score"))
colnames(WHRdata2019)



WHRdata2018=plyr::rename(WHRdata2018, replace = c( "Country.or.region"="Country", 
                                                   "Overall.rank"="Happiness.Rank" ,
                                                   "GDP.per.capita"="Economy..GDP.per.Capita.",
                                                   "Healthy.life.expectancy"="Health..Life.Expectancy.",
                                                   "Freedom.to.make.life.choices"="Freedom",
                                                   "Perceptions.of.corruption"="Trust..Government.Corruption.",
                                                   "Social.support"="Family",
                                                   "Score"="Happiness.Score"))
colnames(WHRdata2018)

WHRdata2015=plyr::rename(WHRdata2015, replace = c( "Happiness Rank" = "Happiness.Rank", 
                                                   "Happiness Score" = "Happiness.Score",
                                                   "Economy (GDP per Capita)" = "Economy..GDP.per.Capita.",
                                                   "Health (Life Expectancy)" = "Health..Life.Expectancy.",
                                                   "Trust (Government Corruption)" = "Trust..Government.Corruption.",
                                                   "Dystopia Residual"="Dystopia.Residual"))

colnames(WHRdata2015)



WHRdata2016=plyr::rename(WHRdata2016, replace = c( "Happiness Rank" = "Happiness.Rank", 
                                                   "Happiness Score" = "Happiness.Score",
                                                   "Economy (GDP per Capita)" = "Economy..GDP.per.Capita.",
                                                   "Health (Life Expectancy)" = "Health..Life.Expectancy.",
                                                   "Trust (Government Corruption)"  = "Trust..Government.Corruption.",
                                                   "Dystopia Residual"="Dystopia.Residual"
))
colnames(WHRdata2016)


WHRdata2015<-cbind(Year=2015,WHRdata2015)

WHRdata2016<-cbind(Year=2016,WHRdata2016)

WHRdata2017<-cbind(Year=2017,WHRdata2017)

WHRdata2018<-cbind(Year=2018,WHRdata2018)

WHRdata2019<-cbind(Year=2019,WHRdata2019)

WHRdata2018$Trust..Government.Corruption. = as.numeric(WHRdata2018$Trust..Government.Corruption.)

str(WHRdata2018)



WHRdata20152016<-dplyr::bind_rows(WHRdata2015,WHRdata2016)

WHRdata201520162017<-dplyr::bind_rows(WHRdata20152016,WHRdata2017)

WHRdata20182019<-dplyr::bind_rows(WHRdata2018,WHRdata2019)

WHRdata<-dplyr::bind_rows(WHRdata201520162017,WHRdata20182019)
head(WHRdata)



WHRdata$Happiness.Rank  = as.numeric(WHRdata$Happiness.Rank )

str(WHRdata)



colSums(is.na(WHRdata))



WHRdata = subset(WHRdata, select = -c(Lower.Confidence.Interval,Upper.Confidence.Interval,Dystopia.Residual,Standard.Error,Whisker.high,Whisker.low))

colSums(is.na(WHRdata))



WHRdata$Trust..Government.Corruption.[is.na(WHRdata$Trust..Government.Corruption.)] <- median(WHRdata$Trust..Government.Corruption., na.rm = T)

colSums(is.na(WHRdata))



aggregate(WHRdata$Country, by=list(WHRdata$Year), FUN=length)



Country_2015 = subset(WHRdata, Year == 2015)$Country
Country_2016 = subset(WHRdata, Year == 2016)$Country
Country_2017 = subset(WHRdata, Year == 2017)$Country
Country_2018 = subset(WHRdata, Year == 2018)$Country
Country_2019 = subset(WHRdata, Year == 2019)$Country



common_country =intersect(intersect(intersect(intersect(Country_2015,
                                                        Country_2016),Country_2017),Country_2018),Country_2019)
length(common_country)



WHRdata_cleaned = subset(WHRdata,Country %in% common_country)
print(paste("The amount of rows in the dataset is: ",dim(WHRdata_cleaned)[1]))
print(paste("The amount of columns in the dataset is: ",dim(WHRdata_cleaned)[2]))



common_region <- unique(subset(WHRdata_cleaned, Region!="NA", c(Country, Region)))

head(common_country)



assign_region <- function(x){
  Region <- common_region$Region[common_region$Country == x]
}

for(country in common_country)
  WHRdata_cleaned$Region[WHRdata_cleaned$Country == country] <- assign_region(country)



WHRdata_cleaned=plyr::rename(WHRdata_cleaned, replace = c( 
  "Economy..GDP.per.Capita."="GDP.per.Capita",
  "Health..Life.Expectancy."="Healthy.Life.Expectancy",
  "Trust..Government.Corruption."="Perceptions.of.Corruption",
  "Family"="Social.Support"))
colnames(WHRdata_cleaned)




skimr::skim_without_charts(WHRdata_cleaned)





DT::datatable(WHRdata_cleaned, class= "compact")



write_csv(WHRdata_cleaned, file = "World Happiness Data (2015-2019)_cleaned.csv")
