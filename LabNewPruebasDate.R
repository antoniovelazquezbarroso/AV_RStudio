library(tidyverse)
library(lubridate)
library(readxl)


NewMovs <- read_excel("data/20220901_20240831.xlsx") # Movimientos 01SEP2022-31AGO2024
                                                     # Por fecha operación orden inverso

# Comprobación de integridad del fichero por saldo
SaldoInicial <- first(NewMovs$Saldo)
SaldoFinal <- last(NewMovs$Saldo)
Cobros <- NewMovs %>% 
          filter(Importe>0) %>% 
          summarise(sum(Importe))
Pagos <-  NewMovs %>% 
          filter(Importe<0) %>% 
          summarise(sum(Importe)) 

CheckSaldo <- SaldoInicial
              -SaldoFinal
              + Cobros
              - Pagos
CheckSaldo == first(NewMovs$Saldo) # Debe ser TRUE


# Elimina columnas innecesarias, cambia nombres de columnas incómodos
CleanMovs <- NewMovs %>% select(-`Fecha Valor`,-`Divisa...5`,-`Divisa...7`)  
names(CleanMovs)[1] = "Fecha"
names(CleanMovs)[5] = "Codigo"

CleanMovs

# Convierte a Date la columna Fecha,
# incluye orden de movimientos del banco,
# añade la Descripcion del Codigo
# ordena por Fecha y NumOrden
# 
ChangedMovs <- CleanMovs %>% 
          mutate(
                 Fecha = parse_date(Fecha, "%d/%m/%Y"),
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

# Códigos, Filtrados los códigos realmente existentes en Movs, y
# Cargadas desde el Excel sus descripciones, y
# Ordenados por número de Codigo
Cods <- as_tibble_col(unique(CleanMovs$Codigo), column_name = "Codigo")
Cods

Codigos <-  Cods %>% 
            left_join(unique(read_excel("data/Cods.xlsx")), by = "Codigo") %>% 
            arrange(Codigo)
Codigos





