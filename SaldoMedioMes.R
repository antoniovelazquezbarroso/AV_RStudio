source("ProcesaBS.R")
#source("ProcesaING.R")                      # Alternativamente


# CÁLCULO DE SALDO MEDIO MENSUAL, TRIMESTRAL, ...

SALDOS <- BS %>% group_by(Fecha) %>%
#SALDOS <- ING %>% group_by(Fecha) %>%        # Alternativamente
  summarize(SaldoFinDia = Saldo[NumOrden == last(NumOrden)]) %>% 
  mutate(FechaAnterior = as_date(
                                 ifelse(Fecha == first(Fecha),
                                        Fecha - day(Fecha), # Último mes anterior
                                        #as_date(floor_date(Fecha, unit = "month") - 1),
                                        lag(Fecha))),
         #ultimo_mes_anterior = Fecha - day(Fecha),
         #primero_mes = Fecha - day(Fecha) + 1,
         #otro_primero_mes = floor_date(Fecha, unit = "month"),                       
         #otro_ultimo_mes_anterior = as_date(floor_date(Fecha, unit = "month")-1),
         #fin_de_mes= as_date(ceiling_date(Fecha, unit = "month")-1),
         DiasSaldo = ifelse(Fecha == first(Fecha), day(Fecha), Fecha - FechaAnterior),
         #OtroDiasSaldo = ifelse(!is.na(lag(Fecha)), as.numeric(Fecha - FechaAnterior),day(Fecha))
        ) %>% select(Fecha, SaldoFinDia, DiasSaldo)
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
