# EXPORTANDO UN TIBBLE COMO EXCEL
library(readxl)
library(writexl)

# Movimientos 01SEP2022-31AGO2024 por fecha operación orden inverso
Entrada <- read_excel("data/ING_01Oct2021_30Sep2024.xlsx")

# Exportando a Excel
TRANS <- Entrada
write_xlsx(TRANS, 'data/TRANS_MovsING.xlsx')
rm(Entrada, TRANS)

# REPORT
source("REPORT_ING.R")

#  TRASPONER ESTE DATA FRAME PARA EXPORTAR A EXCEL
TO_EXCEL <- pivot_longer(REPORT,cols = (-Fecha_Final), names_to="Concepto") %>% 
            pivot_wider(names_from=c(Fecha_Final))
write_xlsx(TO_EXCEL, 'data/TO_EXCEL.xlsx')
rm(TO_EXCEL)

detach("package:writexl", unload = TRUE)

#==============================================================================

#           TRASPONIENDO REPORT (PIVOTING LONGER AND THEN WIDER)

TO_EXCEL <- REPORT

# EN 1 PASO
TO_EXCEL <- pivot_longer(TO_EXCEL, cols = (-Fecha_Final), names_to="name") %>% 
  pivot_wider(names_from=c(Fecha_Final))

# EN 2 PASOS
# Trasponer
TO_EXCEL <- pivot_longer(TO_EXCEL, cols = (-Fecha_Final))#, names_to="name") %>%  
TO_EXCEL <- pivot_wider(TO_EXCEL, names_from=c(Fecha_Final))

# EN 2 PASOS
# Hacer el cambio y luego deshacerlo
TO_EXCEL <- pivot_longer(TO_EXCEL, cols = (-Fecha_Final))#, names_to="name") %>%
TO_EXCEL <- pivot_wider(TO_EXCEL, names_from="name")

#==============================================================================






