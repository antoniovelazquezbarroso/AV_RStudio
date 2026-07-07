# IMPORTA DESDE EXCEL MOVIMIENTOS BS  (Entrada) Y LOS TRANSFORMA (BS)
library(tidyverse)
#library(lubridate) # NO HAY QUE LLAMARLO APARTE, VA CON TIDYVERSE
library(readxl)

# Movimientos por fecha operación orden inverso
Entrada <- read_excel("data/BS_02Sep2022_31Mar2026.xlsx", skip = 7) 

    #> names(Entrada)
    #[1] "FECHA OPERACIÓN" "FECHA VALOR"     "CONCEPTO"        "IMPORTE EUR"     "SALDO"

# EN UN PASO
# Elimina columnas innecesarias, cambia nombres de columnas incómodos
# Convierte a tipo Date la columna Fecha, incluye NumOrden movimientos del banco,
# añade la Descripcion del Codigo y ordena por Fecha y NumOrden
BS <- Entrada %>%
  select(-`FECHA VALOR`) %>%                                          # Elimina columnas innecesarias
  rename( Fecha = `FECHA OPERACIÓN`,
          Concepto = CONCEPTO ,
          Importe = `IMPORTE EUR`,
          Saldo = SALDO) %>%                           # Cambia nombres de columnas incómodos
  mutate(
         #OtraFecha = dmy(Fecha),
         Fecha = parse_date(Fecha, "%d/%m/%Y"),                  # Convierte a tipo Date la columna Fecha
         #TextoFecha = format(Fecha, "%d/%m/%Y"),
         NumOrden = (dim(Entrada)[1] - row_number( ) + 1)        # Incluye NumOrden movimientos del banco
        ) %>% 
  arrange(Fecha, NumOrden) %>%                                       # Ordena Entradas por Fecha y NumOrden
  select(Fecha, NumOrden, Importe, Saldo, Concepto)   # Ordena columnas de modo comodo
BS
    # names(BS)
    #[1] "Fecha"       "NumOrden"    "Importe"     "Saldo"       "Concepto"

rm(Entrada)

