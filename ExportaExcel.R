# EXPORTANDO UN TIBBLE COMO EXCEL
library(readxl)
library(writexl)
# Movimientos 01SEP2022-31AGO2024 por fecha operación orden inverso
Entrada <- read_excel("data/MovsING_20220901_20240922.xlsx")
TRANS <- Entrada
write_xlsx(TRANS, 'data/TRANS_MovsING.xlsx')
rm(Entrada, TRANS)

detach("package:writexl", unload = TRUE)

