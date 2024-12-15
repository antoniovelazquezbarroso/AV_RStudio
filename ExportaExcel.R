# EXPORTANDO UN TIBBLE COMO EXCEL
library(readxl)
library(writexl)
# Movimientos 01SEP2022-31AGO2024 por fecha operación orden inverso
Entrada <- read_excel("data/ING_01Oct2021_30Sep2024.xlsx")
TRANS <- Entrada
write_xlsx(TRANS, 'data/TRANS_MovsING.xlsx')
rm(Entrada, TRANS)

detach("package:writexl", unload = TRUE)

