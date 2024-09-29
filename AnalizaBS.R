source("ProcesaBS.R")

n_meses <- ceiling(interval(first(BS$Fecha),last(BS$Fecha))/dmonths(1))
n_meses                              # Para varios cálculos luego

Total <- BS %>% summarize(
                            numero = n(),
                            inicial = first(Fecha),
                            final = last(Fecha),
                            n_cobros = sum(Importe>0),
                            tot_cobros = sum(Importe[Importe>0]),
                            cobro_medio = mean(Importe[Importe>0]),
                            n_pagos = sum(Importe<0),
                            tot_pagos = sum(Importe[Importe<0]),
                            pago_medio = mean(Importe[Importe<0]),
                            varsaldo = tot_cobros + tot_pagos                            
                            )
Total

IngresoMedioMensual <- sum(BS$Importe[BS$Importe>0])/n_meses
IngresoMedioMensual

GastoMedioMensual <- sum(BS$Importe[BS$Importe<0])/n_meses
GastoMedioMensual

VarSaldoMedioMensual <- IngresoMedioMensual + GastoMedioMensual
VarSaldoMedioMensual    # Ver más abajo otro cálculo, y su desviación típica

# Estadística general por meses
Mensual <- BS %>% group_by(Año = year(Fecha), Mes = month(Fecha)) %>% 
                  summarize(
                            n_BS = n(),
                            n_cobros = sum(Importe>0),
                            tot_cobros = sum(Importe[Importe>0]),
                            cobro_medio = mean(Importe[Importe>0]),
                            n_pagos = sum(Importe<0),
                            tot_pagos = sum(Importe[Importe<0]),
                            pago_medio = mean(Importe[Importe<0]),
                            varsaldo = tot_cobros + tot_pagos,
                            fin_de_mes = max(Fecha)#, Sirve para escala de gráficos
                            #otro_fin_de_mes = ceiling_date((make_date(Año, Mes, 1)), unit = "month") - 1
                           ) %>% 
                  select(Año, Mes, n_BS, varsaldo, everything()) %>% 
                  print(n = nrow(BS))
Mensual

mean(Mensual$varsaldo)
sd(Mensual$varsaldo)

mean(Mensual$tot_cobros)
sd(Mensual$tot_cobros)

mean(Mensual$tot_pagos)
sd(Mensual$tot_pagos)



ggplot(Mensual) + # GRÁFICO  POR MESES CON INGRESOS, GASTOS Y VARSALDO (Y SUS PROMEDIOS)
  geom_line(aes(fin_de_mes, tot_cobros), colour="BLUE") +
  geom_line(aes(fin_de_mes, mean(tot_cobros)), colour="BLUE",linetype = "dotted") +
  geom_line(aes(fin_de_mes, abs(tot_pagos)), colour="RED") +
  geom_line(aes(fin_de_mes, mean(abs(tot_pagos))), colour="RED",linetype = "dotdash") +
  geom_line(aes(fin_de_mes, mean(varsaldo)), colour="GREEN") +
  geom_line(aes(fin_de_mes, mean(varsaldo)), colour="GREEN",linetype = "dotted")

Total

ggplot(BS, aes(Importe)) +         # Histograma de movimientos por Importe
  geom_histogram(bins = 500)

ggplot(BS, aes(Fecha, Saldo)) +    # Historico de saldos por Fecha
  geom_line()

ggplot(BS, aes(Fecha, Importe, colour = (Importe>0))) +
  geom_point()                       # Ingresos y gastos por Fecha
# Tengo que clasificar los movimientos y colorear (viajes, Pilar, Comunidad, ...)

Cobros <- BS %>% filter(Importe>0)
ggplot(Cobros, aes(Importe)) +         # Histograma de Ingresos por Importe
  geom_histogram(bins = 500)

Cobros %>% arrange(desc(Importe)) %>% print(n=20) # Los 20 mayores
count(Cobros)
sum(Cobros$Importe)

Pagos <- BS %>% filter(Importe<0)
ggplot(Pagos, aes(abs(Importe))) +         # Histograma de Pagos por Importe
  geom_histogram(bins = 500)

ggplot(Pagos, aes(Fecha, abs(Importe))) + # Pagos por Fecha
geom_point()

Pagos %>% arrange(Importe) %>% print(n=20) # Los 20 mayores
                                           # Los 20 mayores quitando Cajeros y Recibos Raimundo ¡¡VIAJES!!
Pagos %>% filter(!grepl("Recibo", Concepto), !grepl("Reint", Concepto)) %>% arrange(Importe) %>% print(n=20)

Tarjetas <- BS %>% filter(Codigo=="136")    
count(Tarjetas)
max(abs(Tarjetas$Importe))
Tarjetas %>% filter(near(abs(Importe), max(abs(Importe)), tol = 0.01))
Tarjetas %>% select(Importe, Fecha,Concepto) %>% arrange(Importe, Fecha) %>% print(n=20) # Los 20 mayores
Tarjetas %>% filter(abs(Importe) == max(abs(Importe)))
mean(abs(Tarjetas$Importe))
min(abs(Tarjetas$Importe))

Cajeros <-  BS %>% filter(Codigo=="136", grepl("Reintegro, Atm", Concepto)| grepl("Reint. Cajero", Concepto))
count(Cajeros)
count(Cajeros)/n_meses
sum(Cajeros$Importe)
sum(Cajeros$Importe)/n_meses
mean(Cajeros$Importe)
Cajeros %>% group_by(Año = year(Fecha) ,Mes = month(Fecha)) %>% 
            summarise(n(),
                      sum(Importe),
                      mean(Importe),
                      ) %>% print(n=nrow(Cajeros))


ggplot(Cajeros, aes(abs(Importe))) +         # Histograma de retiradas cajero por Importe
  geom_histogram(bins = 500)

ggplot(Cajeros, aes(Fecha, Importe)) +    # Historico de retiradas cajero por Fecha
  geom_col()


Comunidad <- BS %>% filter(
                           Codigo=="174" & grepl("Geminis", Concepto)|
                           Codigo=="174" & grepl("Cp Rfv 44", Concepto)|
                           Codigo=="174" & grepl("Recibo Raimundo Fernandez", Concepto)
                           )
Comunidad %>% print(n=nrow(Comunidad))

# Disparate de evolución Gastos de Comunidad
ggplot(Comunidad,aes(Fecha,abs(Importe))) +
  geom_point()

Comunidad %>% group_by(Año= year(Fecha), Mes=month(Fecha)) %>%  summarize(
  Num = n(),
  Total = sum(Importe),
  Garaje = sum(Importe[grepl("Geminis", Concepto)])
) %>% print(n=nrow(Comunidad))

Ingresos_Transferencia <- BS %>% filter(Codigo=="071")
count(Ingresos_Transferencia)

Ingresos_Transferencia <- BS %>% filter(Codigo=="071", grepl("Casa",Concepto))
count(Ingresos_Transferencia)

Ingresos_Transferencia <- BS %>% filter(Codigo=="071", grepl("Por Mari",Concepto))
count(Ingresos_Transferencia)

Ingresos_Transferencia <- BS %>% filter(Codigo=="071", grepl("De Antonio Velazquez",Concepto))
count(Ingresos_Transferencia)

Ingresos_Transferencia %>% print(n=nrow(Ingresos_Transferencia))


LAB <- BS %>%
#  group_by(Año = year(Fecha), Mes = month(Fecha), Codigo, Descripcion) %>% 
       group_by(Año = year(Fecha), Mes = month(Fecha)) %>% 
       summarise(num = n(),
                 S_Inicial = Saldo[NumOrden == first(NumOrden)]- Importe[NumOrden == first(NumOrden)],
                 S_Final = Saldo[NumOrden == last(NumOrden)],                 
                 Gastos = sum(Importe[Importe<0]),
                 Ingresos = sum((Importe[Importe>0])),
                 Varsaldo = sum(Importe),                 
#                 xck=n()-sum(Importe<0)-sum(Importe>0) # Debe ser cero
                 n_tarjeta = sum(Codigo == "136"),
                 tarjeta = sum(Importe[Codigo == "136"]),
#                 tarjeta_Ant = sum(Importe[Codigo=="136" & grepl(955303, Concepto)]),
#                 tarjeta_Eva = sum(Importe[Codigo=="136" & grepl(174534, Concepto)]),
#                 xck2 = near((tarjeta - tarjeta_Ant -tarjeta_Eva), 0),
                 n_recibos = sum(Codigo == "174"),
                 recibos = sum(Importe[Codigo == "174"]),
                 Comunidad = sum(Importe[Codigo=="174" & grepl("Geminis", Concepto)]) +
                                     sum(Importe[Codigo=="174" & grepl("Cp Rfv 44", Concepto)]) +
                                     sum(Importe[Codigo=="174" & grepl("Recibo Raimundo Fernandez", Concepto)]),
                 Yoigo = sum(Importe[Codigo=="174" & grepl("Yoigo", Concepto)]),
                 Luz = sum(Importe[Codigo=="174" & grepl("Naturgy", Concepto)]) +
                       sum(Importe[Codigo=="174" & grepl("Gesternova", Concepto)])
                )
LAB
LAB %>% print(n=nrow(LAB))


mean(LAB$Ingresos)
mean(LAB$Gastos)
max(LAB$Gastos)
min(LAB$Gastos)



LAB %>% summarise(n())                  # ENSEÑA FILAS POR AÑO. ??????
LAB %>% summarise(MaxGasto = max(LAB$gastos),
                  MedGasto = mean(LAB$gastos),
                  MinGasto = min(LAB$gastos)
                 )


LAB %>% select(Año, Mes, Comunidad) %>% print(n=24)
LAB$Comunidad
mean(LAB$Comunidad)
mean(LAB$Comunidad[LAB$Año == 2023])
mean(LAB$Comunidad[LAB$Año == 2024])



LAB <- BS %>%
  group_by(year(Fecha), month(Fecha), Codigo, Descripcion) %>%  
  summarise(
    n(),
    sum(Importe),
    max(Importe),
    min(Importe),
    mean(Importe),
    (sum(Importe)/n()),
    first(NumOrden),
    Saldo[NumOrden == first(NumOrden)],
    Importe[NumOrden == first(NumOrden)],
    Inicial = Saldo[NumOrden == first(NumOrden)] 
    - Importe[NumOrden == first(NumOrden)],
    min(Fecha),
    first(Fecha)
  ) %>%
  arrange(desc(`sum(Importe)`))
LAB 

