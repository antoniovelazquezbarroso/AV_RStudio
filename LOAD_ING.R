# IMPORTA DESDE EXCEL MOVIMIENTOS ING (Entrada) Y LOS TRANSFORMA (ING)

library(tidyverse)
#library(lubridate) # NO HAY QUE LLAMARLO APARTE, VA CON TIDYVERSE
library(readxl)

# Movimientos 01SEP2022-31DICIEMBRE2024 por fecha operación orden inverso
Entrada <- read_excel("data/ING_01Oct2021_30Sep2025.xlsx", skip = 4) 

   #> names(Entrada)
   #[1] "F. VALOR"     "CATEGORÍA"    "SUBCATEGORÍA" "DESCRIPCIÓN"  "COMENTARIO"   "IMPORTE (€)"

# EN UN PASO
# Elimina columnas innecesarias,
# Cambia nombres de columnas incómodos,
# Convierte a tipo Date la columna Fecha, Calcula e Incluye NumOrden
# Ordena por Fecha y NumOrden,
# Calcula Saldo para cada movimiento, acumulando desde el inicial
ING <- Entrada %>%
  select(-COMENTARIO) %>%
  rename( Fecha = `F. VALOR`,
          Categoria = CATEGORÍA,
          Subcategoria = SUBCATEGORÍA,
          Descripcion = DESCRIPCIÓN,
          Importe = `IMPORTE (€)`
         ) %>%
  mutate(
         Fecha = as_date(Fecha, tz=NULL),
         #TextoFecha = format(Fecha, "%d/%m/%Y"),
         NumOrden = (dim(Entrada)[1] - row_number( ) + 1)
        ) %>% 
  arrange(Fecha, NumOrden) %>% 
  mutate(
         Saldo = cumsum(Importe) + 21721.76  # €21.721,76  saldo inicial del 1 Octubre 2.021
         ) %>% 
  select(Fecha, NumOrden, Importe, Saldo,Descripcion, Categoria, Subcategoria)

    #> names(ING)
    #[1] "Fecha"        "NumOrden"     "Importe"      "Saldo"        "Descripcion"  "Categoria"    "Subcategoria"

rm(Entrada)
