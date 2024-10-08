# IMPORTA DESDE EXCEL MOVIMIENTOS BS  (Entrada) Y LOS TRANSFORMA (BS)
library(tidyverse)
library(lubridate)
library(readxl)

# Movimientos 01SEP2022-31AGO2024 por fecha operación orden inverso
Entrada <- read_excel("data/20220901_20240831.xlsx") 

    #> names(Entrada)
    #[1] "Fecha Operación" "Fecha Valor"     "Concepto"        "Importe"         "Divisa...5"      "Saldo"          
    #[7] "Divisa...7"      "Código"
    
# EN UN PASO
# Elimina columnas innecesarias, cambia nombres de columnas incómodos
# Convierte a tipo Date la columna Fecha, incluye NumOrden movimientos del banco,
# añade la Descripcion del Codigo y ordena por Fecha y NumOrden
BS <- Entrada %>%
  select(-`Fecha Valor`,-`Divisa...5`,-`Divisa...7`) %>%
  rename( Fecha = `Fecha Operación`, Codigo = Código) %>%
  mutate(
         Fecha = parse_date(Fecha, "%d/%m/%Y"),
         #TextoFecha = format(Fecha, "%d/%m/%Y"),
         NumOrden = (dim(Entrada)[1] - row_number( ) + 1)
        ) %>% 
  left_join(unique(read_excel("data/Cods.xlsx")), by = "Codigo") %>% 
  arrange(Fecha, NumOrden) %>% 
  select(Fecha, NumOrden, Importe, Saldo, Codigo, Descripcion, Concepto)

    # names(BS)
    #[1] "Fecha"       "NumOrden"    "Importe"     "Saldo"       "Codigo"      "Descripcion" "Concepto"

rm(Entrada)
