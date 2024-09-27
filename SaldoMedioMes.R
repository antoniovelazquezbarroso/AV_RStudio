source("ProcesaBS.R")
# CÁLCULO DE SALDO MEDIO MENSUAL, TRIMESTRAL, ...

SALDOS <- BS %>% group_by(Fecha) %>%
  summarize(SaldoFinDia = Saldo[NumOrden == last(NumOrden)]) %>% 
  mutate(FechaAnterior = as_date(
    ifelse(Fecha == first(Fecha),
           Fecha - day(Fecha), # Último del mes anterior
           #as_date(floor_date(Fecha, unit = "month") - 1),
           lag(Fecha))),
    #primero_mes = Fecha - day(Fecha) + 1,
    #ultimo_mes_anterior = Fecha - day(Fecha),
    #otro_primero_mes = floor_date(Fecha, unit = "month"),                       
    #otro_ultimo_mes_anterior = as_date(floor_date(Fecha, unit = "month")-1),
    DiasSaldo = ifelse(Fecha == first(Fecha), day(Fecha), Fecha - FechaAnterior),
    #OtroDiasSaldo = ifelse(!is.na(lag(Fecha)), as.numeric(Fecha - FechaAnterior) , day(Fecha))
  )
SALDOS

SaldoMedioMes <- SALDOS %>% group_by(Año = year(Fecha), Mes = month(Fecha)) %>% 
  summarize(#n(),
            mean(SaldoFinDia),
            weighted.mean(SaldoFinDia, DiasSaldo), # Saldo medio bién calculado
            max(DiasSaldo),
            max(SaldoFinDia),
            min(SaldoFinDia)
  )
SaldoMedioMes
