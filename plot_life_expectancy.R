library(tidyverse)

expectancy %>%
  filter(race == "All Races",
         sex == "Both Sexes") %>%
  ggplot(aes(year, life_expectancy)) +
    geom_line()

expectancy %>%
  filter(race == "All Races") %>%
  ggplot(aes(year, life_expectancy)) +
  geom_line(aes(color = sex))
