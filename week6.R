data1 <- read.csv(file.choose(), header = TRUE)

data2 <- read.table(file.choose(), header = T, sep = ",")

mydata <- read.xlsx("C:/Users/n9796134/Downloads/file_example_XLS_100.xls", 1)

View(gapminder)

summary(gapminder)

mean(gapminder$gdpPercap)

attach(gapminder)

median(lifeExp)
#plot
hist(lifeExp)
hist(pop)
hist(log(pop))
boxplot(lifeExp ~ continent)
plot(lifeExp ~ log(gdpPercap))

#manipulate
library(dplyr)
gapminder %>%
  select(country, lifeExp) %>%
  filter(country=="South Africa" | country=="Ireland") %>%
  group_by(country) %>%
  summarise(Average_life = mean(lifeExp))

#data visualisation

library(ggplot2)
gapminder %>%
  filter(gdpPercap < 50000) %>%
  ggplot(aes(x=log(gdpPercap), y=lifeExp,col=year,  size = pop))+geom_point(alpha=0.3)+geom_smooth(method = lm) + facet_wrap(~continent)

summary(mtcars)

mydata <- mtcars[, c('mpg', 'cyl', 'disp', 'hp', 'carb')]
summary(mydata)

library(PerformanceAnalytics)
chart.Correlation(mydata, histogram=TRUE, pch=19)

library(corrplot)
corrplot.mixed(cor(mydata), order="hclust", tl.col="black")

library(corrr)
mydata %>% 
  correlate() %>%
  network_plot(min_cor=0.6)