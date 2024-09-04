trend <- expectancy
trend <- trend[trend$race == "All Races", ]
trend <- trend[trend$sex == "Both Sexes", ]
trend$race <- NULL
trend$sex <- NULL
trend$death_rate <- NULL

plot(trend)

rm(trend)
