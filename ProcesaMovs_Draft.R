library(tidyverse)
library(lubridate)
library(readxl)

# Movimientos 01SEP2022-31AGO2024 por fecha operación orden inverso
NewMovs <- read_excel("data/20220901_20240831.xlsx") 

NewMovs                                                     

# Elimina columnas innecesarias, cambia nombres de columnas incómodos
CleanMovs <- NewMovs %>% select(-`Fecha Valor`,-`Divisa...5`,-`Divisa...7`)  
names(CleanMovs)[1] = "Fecha"
names(CleanMovs)[5] = "Codigo"

CleanMovs
rm(NewMovs)

# Convierte a tipo Date la columna Fecha,
# incluye NumOrden de movimientos del banco,
# añade la Descripcion del Codigo
# ordena por Fecha y NumOrden
ChangedMovs <- CleanMovs %>% 
  mutate(
    Fecha = parse_date(Fecha, "%d/%m/%Y"),
    #OtraFormaFecha = as.Date(Fecha, "%d/%m/%Y"),
    #TextoFecha = format(Fecha, "%d/%m/%Y"),
    NumOrden = (dim(CleanMovs)[1] - row_number( ) + 1)#,
    #MonthDay = mday(Fecha),
    #Month = month(Fecha),
    #Year = year(Fecha)
  ) %>% 
  left_join(unique(read_excel("data/Cods.xlsx")), by = "Codigo") %>% 
  arrange(Fecha, NumOrden)%>% 
  select(Fecha, NumOrden, Importe, Saldo, Codigo, Descripcion, Concepto)

ChangedMovs

Movs <- ChangedMovs   # Cortito
rm(CleanMovs, ChangedMovs)