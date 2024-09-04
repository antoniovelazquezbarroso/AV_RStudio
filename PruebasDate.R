library(tidyverse)
library(lubridate)
library(readxl)

library(nycflights13)


Movs <- read_excel("data/Movs.xlsx") # Movimientos 01JUL2022-31JUL2024
                                     # Por fecha operación orden inverso

Unicods <- unique(read_excel("data/Cods.xlsx")) # Elimina los repetidos, 
                                                # se usa para crear Cods

# A partir de Movs y Unicods
Cods <- as_tibble_col(unique(Movs$Codigo), column_name = "Codigo") %>% 
  left_join(Unicods, by = "Codigo") %>% 
  arrange(Codigo)


RawMovs <- Movs
names(RawMovs)
CleanMovs <-  RawMovs %>% 
              mutate(Date=parse_date(Fecha, "%d/%m/%Y"),
                     TextDate=dmy(Date),
                     StringDate= mday(Date)/month(Date)/year(Date),
                     MonthDay=mday(Date),
                     Month=month(Date),
                     Year=year(Date)
                     ) 
CleanMovs
