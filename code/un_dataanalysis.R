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

joined_co2_pop <- inner_join(co2_emissions, gapminder_data_2007, by = "country")

##anti_join finds values OUT OF COMMON

anti_join(co2_emissions, gapminder_data_2007, by = "country")

full_join(co2_emissions, gapminder_data_2007) %>% View()

write_csv(joined_co2_pop, file = "data/joined_co2_pop.csv")

joined_co2_pop <- read_csv("data/joined_co2_pop.csv")

ggplot(data = joined_co2_pop) +
  aes(x = country, y = gdpPercap) +
  labs(x = "Country", y = "GDP per Capita") +
  geom_boxplot()

ggplot(data = joined_co2_pop) +
  aes(x = lifeExp) +
  labs(x = "Life Expectancy (Years)", y = "Frequency") +
  geom_histogram(bins = 50, fill = "cornflowerblue") +
  ##scale_color_brewer(palette = "Set2") +
  theme_prism() 

install.packages("ggthemes")
library("ggthemes")
install.packages("ggprism")
library("ggprism")

ggplot(data = joined_co2_pop) +
  aes(x = gdpPercap) +
  labs(x = "GDP per Capita (USD)", y = "Frequency") +
  geom_histogram(bins = 50, fill = "pink") +
  theme_prism()

gdp_co2_plot <- joined_co2_pop %>% 
  ggplot(aes(x = gdpPercap, y = per_capita_emissions)) +
  labs (x = "GDP per Capita (USD)", y = "Per Capita CO2 Emissions (metric tons)", title = "It's possible to have high GDP and low CO2 emissions!") +
  geom_point(color = "cornflowerblue") +
 ## xlim(0, 50000) +
  ##ylim(0, 30) +
  geom_smooth(color = "pink", method = "lm") + ##se = FALSE to get rid of gray shadow
  labs(caption = "Pink line represents standard linear regression model.") +
  theme_classic() +
  ggpubr::stat_regline_equation(aes(label = after_stat(rr.label))) +
  theme(plot.background = element_rect(fill = "pink")) 
 

## ex. annotate('text', x = 3, y = 13.5, label = 'C')
## annotate('text', x = 33385, y = 5.438, label = "sverige") +

install.packages("ggpubr")
library(ggpubr)

ggsave(gdp_co2_plot, filename = "figures/gdp_vs_co2_plot.png", 
       height = 4, width = 6, units = "in", dpi = 300)
