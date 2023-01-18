2+2
library("tidyverse")
read_csv(file = "gapminder_1997.csv")
Sys.Date()
getwd()
Sys.Date()
sum(5,6)
?round()
round(3.1415)
round(3.1415,3)
round(x = 3.1415)
round(x = 3.1415, digits = 2)
round(digits = 2, x = 3.1415)
round(2, 3.1415)
library(readr)
library(readxl)


#Goal: Analyze life expectancy and C02 emissions vs popul. w gapminder data
#Date: Jan. 17th, 2023
#author: Daniel Tetreault

library("tidyverse")
library(readr)
gapminder_1997 <- read_csv("gapminder_1997.csv")

ggplot(data = gapminder_1997) +
  aes(x = gdpPercap, y = lifeExp) +
  labs(x = "GDP Per Capita", y = "Life Expectancy")

ggplot(data = gapminder_1997) +
  aes(x = gdpPercap) +
  labs(x = "GDP Per Capita") +
  aes(y = lifeExp) +
  labs(y = "Life Expectancy") +
  geom_point() +
  labs(title = "Do people in wealthy countries live longer?") +
  aes(color = continent) +
  labs(color = "Continent") +
  scale_color_brewer(palette = "Set2") +
  aes(size = pop/1000000) +
  labs(size = "Population (in millions)") +
  aes(shape = continent) +
  labs(shape = "Continent")

##This works because you can treat the columns in the aesthetic mappings 
##just like any other variables and can use functions to transform or change
##them at plot time rather than having to transform your data first.

RColorBrewer::display.brewer.all()

#read in all gapminder data
gapminder_data <- read.csv("gapminder_data.csv")

dim(gapminder)

ggplot(data = gapminder_data) +
  aes(x=year, y=lifeExp, color=continent,) +
  scale_color_brewer(palette = "Set2") +
  geom_point()

rm(gapminder)
#gets rid of a data object/variable if you assign an object that later on 
#you don't want to use anymore
str(gapminder_data)

ggplot(data = gapminder_data) +
  aes(x=year, y=lifeExp, color=continent, group = country) +
  scale_color_brewer(palette = "Set2") +
  geom_line()


ggplot(data = gapminder_1997) +
  aes(x=continent, y=lifeExp) +
  geom_boxplot()

ggplot(data = gapminder_1997) +
  aes(x=continent, y=lifeExp) +
  geom_violin(fill ="pink", color = "blue") +
  geom_jitter(aes(size = pop)) 

ggplot(data = gapminder_1997) +
  aes(x=continent, y=lifeExp, color = continent) +
  geom_violin(fill ="pink", color = "blue") +
  geom_jitter(aes(size = pop)) 

  
##jitter is like geom_point but doesn't limit the points to the area of the violins


##univariate plot, histogram

ggplot(data = gapminder_1997) +
  aes(x = lifeExp) +
  geom_histogram(bins = 20) +
  theme_prism()

##can either decrease binwidth = or increase bins = to add more bars to the histogram


install.packages("ggthemes")
library("ggthemes")
install.packages("ggprism")
library("ggprism")

ggplot(gapminder_1997) +
  aes(x = gdpPercap, y = lifeExp) +
  geom_point()+
  facet_wrap(vars(continent))

ggplot(gapminder_1997) +
  aes(x = gdpPercap, y = lifeExp) +
  geom_point()+
  facet_grid(rows = vars(continent))

ggsave("awesome_plot.tiff", units = "in", width = 6, height = 4)
##ggsave() will save the last ggplot rendered

ggsave("figures/awesome_plot.jpg", width=6, height=4)


