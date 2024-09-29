# IMPORTA DESDE EXCEL MOVIMIENTOS ING (Entrada) Y LOS TRANSFORMA (ING)

library(tidyverse)
library(lubridate)
library(readxl)

# Movimientos 01SEP2022-31AGO2024 por fecha operación orden inverso
Entrada <- read_excel("data/MovsING_20220901_20240922.xlsx") 

   #> names(Entrada)
   #[1] "F. VALOR"     "CATEGORÍA"    "SUBCATEGORÍA" "DESCRIPCIÓN"  "COMENTARIO"   "IMAGEN"       "IMPORTE (€)" 
   #[8] "SALDO (€)"

# EN UN PASO
# Elimina columnas innecesarias, cambia nombres de columnas incómodos
# Convierte a tipo Date la columna Fecha, incluye NumOrden movimientos del banco,
# añade la Descripcion del Codigo y ordena por Fecha y NumOrden
ING <- Entrada %>%
  select(-IMAGEN, -COMENTARIO) %>%
  rename( Fecha = `F. VALOR`,
          Categoria = CATEGORÍA,
          Subcategoria = SUBCATEGORÍA,
          Descripcion = DESCRIPCIÓN,
          Importe = `IMPORTE (€)`,
          Saldo = `SALDO (€)`
         ) %>%
  mutate(
         Fecha = as_date(Fecha, tz=NULL),
         #TextoFecha = format(Fecha, "%d/%m/%Y"),
         NumOrden = (dim(Entrada)[1] - row_number( ) + 1)
        ) %>% 
  arrange(Fecha, NumOrden) %>% 
  select(Fecha, NumOrden, Importe, Saldo,Descripcion, Categoria, Subcategoria)

    #> names(ING)
    #[1] "Fecha"        "NumOrden"     "Importe"      "Saldo"        "Descripcion"  "Categoria"    "Subcategoria"

rm(Entrada)

