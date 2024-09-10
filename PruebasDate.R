library(tidyverse)
library(lubridate)
library(readxl)


Movs <- read_excel("data/Movs.xlsx") # Movimientos 01JUL2022-31JUL2024
                                     # Por fecha operación orden inverso

# Filtrados los códigos realmente existentes en Movs, y
# Cargadas desde el Excel sus descripciones, y
# Ordenados por número de Codigo
Cods <- as_tibble_col(unique(Movs$Codigo), column_name = "Codigo") %>%
  left_join(unique(read_excel("data/Cods.xlsx")), by = "Codigo") %>% 
  arrange(Codigo)



names(Movs)
CleanMovs <-  Movs %>% 
              mutate(Date=parse_date(Fecha, "%d/%m/%Y"),
                     OtherDate=as.Date(Fecha, "%d/%m/%Y"),
                     TextDate=format(Date, "%d/%m/%Y"),
                     #StringDate= mday(Date)/month(Date)/year(Date),
                     MonthDay=mday(Date),
                     Month=month(Date),
                     Year=year(Date)
                     ) 
CleanMovs

#Otro similar a MovsByCods, empezando por los movimientos
(
  MovsWithCods <- Movs %>% 
  left_join(Cods, by = "Codigo") %>% 
  select(Dia, Mes, Año, Codigo, Descripcion, Importe, Concepto, FechaNS) %>% 
  arrange(FechaNS)
)

print(MovsWithCods, n=nrow(MovsWithCods))

(
  MovsByMonth <- MovsWithCods %>%
  group_by(Codigo, Descripcion, Año, Mes) %>% 
  summarise(numero = n(),
            total_mes = sum(Importe),
           )
)


# CleanMovsWithCods


CleanMovsWithCods <- CleanMovs %>%
                     left_join(Cods, by = "Codigo") %>% 
                     select(Date,Dia, Mes, Año, Codigo, Descripcion, Importe, Concepto, FechaNS) %>% 
                     arrange(FechaNS)

