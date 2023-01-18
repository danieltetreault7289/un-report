library(tidyverse)
getwd()
gapminder_data <- read_csv("data/gapminder_data.csv")
summarise(gapminder_data, averageLifeExp=mean(lifeExp), 
          medianLifeExp=median(lifeExp))

gapminder_summary <- gapminder_data %>% 
  summarise(averageLifeExp=mean(lifeExp))

##piping assumes that the piped data/object is the first argument into the piped function

gapminder_summary_2007 <- gapminder_data %>% 
  filter(year == 2007) %>% 
  summarize(average = mean(lifeExp))

gapminder_summary_1952 <- gapminder_data %>% 
  filter (year == min(year)) %>% 
  summarize(average = mean(gdpPercap))




