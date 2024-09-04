# Import NCHS Dataset
# File download from:
# https://catalog.data.gov
# https://catalog.data.gov/dataset/nchs-death-rates-and-life-expectancy-at-birth
# Contains life expectancy data from 1900 in the US

library(readr)
expectancy <- read_csv("data/NCHS_-_Death_rates_and_life_expectancy_at_birth.csv")

#View(expectancy)

# Clean up dataframe
# Use shorter column names
names(expectancy)[1] <- "year"
names(expectancy)[2] <- "race"
names(expectancy)[3] <- "sex"
names(expectancy)[4] <- "life_expectancy"
names(expectancy)[5] <- "death_rate"







