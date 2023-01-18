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

gapminder_data %>% 
  group_by(year) %>% 
  summarize(average = mean(lifeExp),
            error = sd(lifeExp))

gapminder_data %>% 
  mutate(gdp = pop * gdpPercap, popInMillions = pop / 1000000)

gapminder_data %>% 
  select(pop, year)

gapminder_data %>% 
  select(-continent)

gapminder_data %>% 
  select(country, continent, year, lifeExp) %>%
  pivot_wider(names_from = year, values_from = lifeExp) %>% 
  View()

co2_emissions_dirty <- read_csv("co2-un-data.csv", skip = 2, 
         col_names = c("region", "country", "year", "series", "value", "footnotes", "source"))

#in R, a function separates arguments with commas. Note the use of c() and how it eats its arguments

co2_emissions <- co2_emissions_dirty %>% 
  select(country, year, series, value) %>% 
  mutate(series = recode(series, 
                         "Emissions (thousand metric tons of carbon dioxide)" = "total_emissions",
                         "Emissions per capita (metric tons of carbon dioxide)" = "per_capita_emissions")) %>% 
  pivot_wider(names_from = series, values_from = value) %>% 
  filter(year == 2005) %>% 
  select(-year)

gapminder_data_2007 <- read_csv("data/gapminder_data.csv") %>% 
  filter(year == 2007) %>% 
  select(country, pop, lifeExp, gdpPercap)

inner_join(co2_emissions, gapminder_data_2007, by = "country")

##anti_join finds values OUT OF COMMON

anti_join(co2_emissions, gapminder_data_2007, by = "country")

full_join(co2_emissions, gapminder_data_2007) %>% View()

