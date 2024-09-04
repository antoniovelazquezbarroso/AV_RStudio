# Import  Movs Dataset
# File download from:


library(readr)
Rawmovs <- read_excel("data/Movs.xlsx")



dim(Rawmovs)[1]

ggMovs <- Rawmovs %>% mutate(
                             MiFecha = parse_date(Fecha, "%d/%m/%Y"),
                             MiNumOrden = (dim(Rawmovs)[1] - row_number( ) + 1)
                             )

ggMovs <-  arrange(ggMovs, MiNumOrden)





#View(expectancy)

# Clean up dataframe
# Use shorter column names
names(expectancy)[1] <- "year"
names(expectancy)[2] <- "race"
names(expectancy)[3] <- "sex"
names(expectancy)[4] <- "life_expectancy"
names(expectancy)[5] <- "death_rate"







