# IMPORTA DESDE EXCEL MOVIMIENTOS BS  (Entrada) Y LOS TRANSFORMA (BS)
library(tidyverse)
#library(lubridate) # NO HAY QUE LLAMARLO APARTE, VA CON TIDYVERSE
library(readxl)

# Movimientos por fecha operación orden inverso
Entrada <- read_excel("data/BS_02Sep2022_30Sep2025.xlsx", skip = 7) 

    #> names(Entrada)
    #[1] "Fecha Operación"       "Fecha Valor"           "Concepto"              "Importe"               "Divisa...5"            "Saldo"                
    #[7] "Divisa...7"            "Código"                "Número de documento"   "Referencia 1"          "Referencia 2"          "Información adicional"

# EN UN PASO
# Elimina columnas innecesarias, cambia nombres de columnas incómodos
# Convierte a tipo Date la columna Fecha, incluye NumOrden movimientos del banco,
# añade la Descripcion del Codigo y ordena por Fecha y NumOrden
BS <- Entrada %>%
  select(-`Fecha Valor`,                                          # Elimina columnas innecesarias
         -`Divisa...5`,
         -`Divisa...7`,
         -`Número de documento`,
         -`Referencia 1`,
         -`Referencia 2`,
         -`Información adicional`
        ) %>%
  rename( Fecha = `Fecha Operación`, Codigo = Código) %>%          # Cambia nombres de columnas incómodos
  mutate(
         #OtraFecha = dmy(Fecha),
         Fecha = parse_date(Fecha, "%d/%m/%Y"),                  # Convierte a tipo Date la columna Fecha
         #TextoFecha = format(Fecha, "%d/%m/%Y"),
         NumOrden = (dim(Entrada)[1] - row_number( ) + 1)         # Incluye NumOrden movimientos del banco
        ) %>% 
  left_join(unique(read_excel("data/Cods.xlsx")), by = "Codigo") %>% # Añade la Descripcion del Codigo
  arrange(Fecha, NumOrden) %>%                                       # Ordena Entradas por Fecha y NumOrden
  select(Fecha, NumOrden, Importe, Saldo, Codigo, Descripcion, Concepto)   # Ordena columnas de modo comodo
BS
    # names(BS)
    #[1] "Fecha"       "NumOrden"    "Importe"     "Saldo"       "Codigo"      "Descripcion" "Concepto"

rm(Entrada)

