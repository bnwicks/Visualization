### Section 3: Summarizing with dplyr
# 3.1 Summarizing with dplyr

library(tidyverse)
data(heights)

s <- heights %>%
  filter(sex == "Male") %>%
  summarise(average = mean(height), standard_deviation = sd(height))

s

heights %>%
  filter(sex == "Male") %>%
  summarize(median = median(height),
            minimum = min(height),
            maximum = max(height))

# The Dot Placeholder
data(murders)
murders <- murders %>% mutate(murder_rate = total/population*100000)
summarize(murders, mean(murder_rate))

us_murder_rate <- murders %>%
  summarise(rate = sum(total) / sum(population) *100000) %>%
  .$rate

us_murder_rate

# Group By
murders %>%
  group_by(region) %>%
  summarise(median_rate = median(murder_rate))

# Sorting Data Tables
murders %>% arrange(murder_rate) %>% head()
murders %>% arrange(desc(murder_rate)) %>% head()

# gapminder; Case Study: Trends in World Health and Economics
gapminder %>%
  filter(year == 2015 & country %in% c("Sri Lanka", "Turkey")) %>%
  select(country, infant_mortality)

ds_theme_set()
filter(gapminder, year%in%c(1962, 2012)) %>%
  ggplot(aes(fertility, life_expectancy, color = continent)) +
  geom_point() + 
  facet_grid(.~year)

# Time Series Plots
ds_theme_set()
gapminder %>%
  filter(country == "United States") %>%
  ggplot(aes(year, fertility)) +
  geom_line()

gapminder %>%
  filter(country %in% c("Germany", "South Korea")) %>%
  ggplot(aes(year, fertility, col=country)) +
  geom_line()

# Tranformations
gapminder <- gapminder %>%
  mutate(dollars_per_day = gdp/population/365)

past_year <- 1970
gapminder %>%
  filter(year == past_year & !is.na(gdp)) %>%
  ggplot(aes(log2(dollars_per_day))) +
  geom_histogram(binwidth = 1, color = "black")

# Comparing Distributions
past_year = 1970
present_year = 2010

west <- c("Western Europe", "Northern Europe", "Southern Europe", 
          "Northern America", "Ausrtralia and New Zealand")

gapminder %>%
  filter(year == c(past_year = 1970, present_year = 2010) & !is.na(gdp)) %>%
  mutate(group = ifelse(region%in%west, "West", "Developing")) %>%
  ggplot(aes(dollars_per_day)) +
  geom_histogram(binwidth = 1, color = "black") +
  scale_x_continuous(trans = "log2") +
  facet_grid(year~group)

# Comparing Distributions with only contries excisting in 1970 & 2010
country_list_1 <- gapminder %>%
  filter(year == past_year & !is.na(dollars_per_day)) %>% .$country

country_list_2 <- gapminder %>%
  filter(year == present_year & !is.na(dollars_per_day)) %>% .$country

country_list <- intersect(country_list_1, country_list_2)

gapminder %>%
  filter(year %in% c(past_year, present_year) & country%in%country_list) %>%
  mutate(group = ifelse(region%in%west, "West", "Developing")) %>%
  ggplot(aes(dollars_per_day)) +
  geom_histogram(binwidth = 1, color = "black") +
  scale_x_continuous(trans = "log2") +
  facet_grid(year~group)

# Pretty boxplots
p <- gapminder %>%
  filter(year %in% c(past_year, present_year) & country%in%country_list) %>%
  mutate(group = ifelse(region%in%west, "West", "Developing")) %>%
  ggplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("") +
  scale_y_continuous(trans = "log2") 

p +
  geom_boxplot(aes(region, dollars_per_day, fill = continent)) +
  facet_grid(year~.)

p +
  geom_boxplot(aes(region, dollars_per_day, fill = factor(year)))

# Density Plots
p <- gapminder %>%
  filter(year %in% c(past_year, present_year) & country%in%country_list) %>%
  mutate(group = ifelse(region%in%west, "West", "Developing")) %>%
  ggplot(aes(dollars_per_day, y= ..count.., fill = group)) +
  scale_x_continuous(trans = "log2") 

p +
  geom_density(alpha = 0.2) + facet_grid(year ~ .)

# Stacked Density Plots
p +
  geom_density(alpha = 0.2, bw = 0.75, position = "stack") +
  facet_grid(year ~ .)

# Ecological Fallacy
gapminder <- gapminder %>%
  mutate(group = case_when(
    .$region %in% west ~ "The West",
    .$region %in% "Norhtern Africa" ~ "Northern Africa",
    .$region %in% c("Eastern Asia", "South-Eastern Asia") ~ "East Asia",
    .$region %in% "Southern Asia" ~ "Southern Asia",
    .$region %in% c("Central America", "South America", "Carribbean") ~ "Latin America",
    .$continent == "Africa" & .$region %in% "Northern Africa" ~ "Sub-Saharan Africa",
    .$region %in% c("Melanesia", "Micronesia", "Polynesia") ~ "Pacific Islands"))

surv_income <- gapminder %>%
  filter(year %in% present_year & !is.na(gdp) & !is.na(infant_mortality) & !is.na(group)) %>%
  group_by(group) %>%
  summarise(income = sum(gdp)/sum(population)/356,
            infant_survival_rate = 1 - sum(infant_mortality/1000 * population)/sum(population))


surv_income %>% arrange(income)
