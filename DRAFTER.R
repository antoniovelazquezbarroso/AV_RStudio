library(tidyverse)
library(readxl)
library(writexl)

# Movimientos 01SEP2022-31AGO2024 por fecha operación orden inverso
Entrada <- read_excel("data/MovsING_20220901_20240922.xlsx")

TRANS <- Entrada
write_xlsx(TRANS, 'data/TRANS_MovsING.xlsx')

# SALDO MEDIO MENSUAL
SALDO <- BS %>% group_by(Año = year(Fecha), Mes = month(Fecha), Dia = day(Fecha)) %>% 
                  summarise(total = n(),
                            SaldoFinDia = (Saldo[NumOrden == last(NumOrden)]),
                            NumDiasSaldo =
                            )
SALDO

SALDO %>% group_by(Año, Mes) %>% summarise(mean(SaldoFinDia))


Date1 <- first(BS$Fecha)
Date1
Date2 <- BS$Fecha[BS$NumOrden == 1]
Date2
Date3 <-  BS$Fecha[BS$NumOrden == 4]
Date3




DIFF <- as.numeric(difftime(ymd(Date3),ymd(Date2), units = "days"))
DIFF2 <- as.numeric(Date3 - Date2)