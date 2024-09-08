library(tidyverse)
library(lubridate)
library(readxl)


NewExportMovs <- read_excel("data/20220901_20240831.xlsx") # Movimientos 01JUL2022-31JUL2024
                                                                 # Por fecha operación orden inverso

SaldoInicial <- first(NewExportMovs$Saldo)
SaldoFinal <- last(NewExportMovs$Saldo)
Cobros <- NewExportMovs %>% 
          filter(Importe>0) %>% 
          summarise(sum(Importe))
Pagos <-  NewExportMovs %>% 
          filter(Importe<0) %>% 
          summarise(sum(Importe)) 

CheckSaldo <- SaldoInicial
              -SaldoFinal
              + Cobros
              - Pagos
CheckSaldo == first(NewExportMovs$Saldo) # Debe ser TRUE

# Elimina columnas innecesarias, cambia nombres de columnas incómodos
Clean <- NewExportMovs %>% select(-`Fecha Valor`,-`Divisa...5`,-`Divisa...7`)  
names(Clean)[1] = "Fecha"
names(Clean)[5] = "Codigo"

Clean

# Convierte a Date la columna Fecha,
# incluye orden de movimientos del banco,
# añade la Descripcion del Codigo
# ordena por Fecha y NumOrden
# 
Changed <- Clean %>% 
          mutate(
                 Fecha = parse_date(Fecha, "%d/%m/%Y"),
                 #TextoFecha = format(Fecha, "%d/%m/%Y"),
                 NumOrden = (dim(Clean)[1] - row_number( ) + 1)#,
                 #MonthDay = mday(Fecha),
                 #Month = month(Fecha),
                 #Year = year(Fecha)
                ) %>% 
          left_join(unique(read_excel("data/Cods.xlsx")), by = "Codigo") %>% 
          arrange(Fecha, NumOrden)%>% 
          select(Fecha, Importe, Saldo, Codigo, Descripcion, Concepto)

Changed

# Códigos, Filtrados los códigos realmente existentes en Movs, y
# Cargadas desde el Excel sus descripciones, y
# Ordenados por número de Codigo
Cods <- as_tibble_col(unique(Clean$Codigo), column_name = "Codigo") %>
  left_join(unique(read_excel("data/Cods.xlsx")), by = "Codigo") %>% 
  arrange(Codigo)


RawMovs <-Entrada
CleanMovs <-  RawMovs %>% 
  mutate(Date=parse_date(`Fecha Operación`, "%d/%m/%Y"),
         #OtherDate=as.Date(`Fecha Operación`, "%d/%m/%Y"),
         TextDate=format(Date, "%d/%m/%Y"),
         #StringDate= mday(Date)/month(Date)/year(Date),
         MonthDay=mday(Date),
         Month=month(Date),
         Year=year(Date)
  ) 
CleanMovs


#Otro similar a MovsByCods, empezando por los movimientos
MovsWithCods <- Movs %>% 
  left_join(Cods, by = "Codigo") %>% 
  select(Dia, Mes, Año, Codigo, Descripcion, Importe, Concepto, FechaNS) %>% 
  arrange(FechaNS) %>% 
  group_by(Codigo, Descripcion, Año, Mes) %>% 
  summarise(numero = n(),
            total_mes = sum(Importe),
  )

MovsWithCods
print(MovsWithCods, n=nrow(MovsWithCods))




