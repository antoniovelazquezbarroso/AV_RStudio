source("LOAD_ING.R")

n_meses <- ceiling(interval(first(ING$Fecha),last(ING$Fecha))/dmonths(1))
n_meses                              # Para varios cálculos luego

ING %>% summarize(
                   numero = n(),
                   inicial = first(Fecha),
                   final = last(Fecha),
                   n_cobros = sum(Importe>0),
                   tot_cobros = sum(Importe[Importe>0]),
                   cobro_medio = mean(Importe[Importe>0]),
                   n_nomina = sum(grepl("Nomina recibida", Descripcion)),
                   nomina = sum(Importe[grepl("Nomina recibida", Descripcion)]),
                   n_pagos = sum(Importe<0),
                   tot_pagos = sum(Importe[Importe<0]),
                   pago_medio = mean(Importe[Importe<0]),
                   pago_medio = mean(Importe[Importe<0]),
                   pago_medio = mean(Importe[Importe<0])
                   )

ING %>% group_by(Año = year(Fecha), Mes = month(Fecha)) %>% 
         summarize(
                   n_ING = n(),
                   n_cobros = sum(Importe>0),
                   tot_cobros = sum(Importe[Importe>0]),
                   cobro_medio = mean(Importe[Importe>0]),
                   nomina = sum(Importe[grepl("Nomina recibida", Descripcion)]),
                   n_pagos = sum(Importe<0),
                   tot_pagos = sum(Importe[Importe<0]),
                   pago_medio = mean(Importe[Importe<0]),
                   pago_medio = mean(Importe[Importe<0]),
                   pago_medio = mean(Importe[Importe<0]),
                   varsaldo = tot_cobros + tot_pagos
                  ) %>% 
                   select(Año, Mes, n_ING, varsaldo, everything()) %>% 
                   print(n = nrow(ING))
              

ggplot(ING, aes(Importe)) +         # Histograma de movimientos por Importe
  geom_histogram(bins = 500)

ggplot(ING, aes(Fecha, Saldo)) +    # Historico de saldos por Fecha
  geom_line()

ggplot(ING, aes(Fecha, Importe, colour = (Importe>0))) +
  geom_point()                       # Ingresos y gastos por Fecha

Cobros <- ING %>% filter(Importe>0)
ggplot(Cobros, aes(Importe)) +         # Histograma de Ingresos por Importe
  geom_histogram(bins = 500)

Cobros %>% arrange(desc(Importe)) %>% print(n=nrow(Cobros)) # Los 20 mayores
count(Cobros)
sum(Cobros$Importe)

Pagos <- ING %>% filter(Importe<0)

Pagos %>% filter(Importe>-2000)     # Gastos Corrientes

ggplot(Pagos, aes(abs(Importe))) +         # Histograma de Pagos por Importe
  geom_histogram(bins = 500)

Nomina <- ING %>% filter(grepl("Nomina recibida", Descripcion)) 
Nomina
Nomina %>% summarize(n(),sum(Importe))

ggplot(Pagos, aes(Fecha, abs(Importe))) + # Pagos por Fecha
geom_point()


Pagos %>% arrange(Importe) %>% print(n=20) # Los 20 mayores
#                                              # Los 20 mayores quitando Cajeros y Recibos Raimundo
#   Pagos %>% filter(!grepl("Recibo", Concepto), !grepl("Reint", Concepto)) %>% arrange(Importe) %>% print(n=20)


#=====================================

SaldosMes <- ING %>%
#  group_by(Año = year(Fecha), Mes = month(Fecha), Codigo, Descripcion) %>% 
       group_by(Año = year(Fecha), Mes = month(Fecha)) %>% 
       summarise(num = n(),
                 S_Inicial = Saldo[NumOrden == first(NumOrden)]- Importe[NumOrden == first(NumOrden)],
                 S_Final = Saldo[NumOrden == last(NumOrden)],                 
                 Gastos = sum(Importe[Importe<0]),
                 Ingresos = sum((Importe[Importe>0])),
                 Varsaldo = sum(Importe),                 
#                 xck=n()-sum(Importe<0)-sum(Importe>0) # Debe ser cero
                 #n_tarjeta = sum(Codigo == "136"),
                 #tarjeta = sum(Importe[Codigo == "136"]),
#                 tarjeta_Ant = sum(Importe[Codigo=="136" & grepl(955303, Concepto)]),
#                 tarjeta_Eva = sum(Importe[Codigo=="136" & grepl(174534, Concepto)]),
#                 xck2 = near((tarjeta - tarjeta_Ant -tarjeta_Eva), 0),
                 #n_recibos = sum(Codigo == "174"),
                 #recibos = sum(Importe[Codigo == "174"]),
                 #Comunidad = sum(Importe[Codigo=="174" & grepl("Geminis", Concepto)]) +
                 #                    sum(Importe[Codigo=="174" & grepl("Cp Rfv 44", Concepto)]) +
                 #                    sum(Importe[Codigo=="174" & grepl("Recibo Raimundo Fernandez", Concepto)]),
                 #Yoigo = sum(Importe[Codigo=="174" & grepl("Yoigo", Concepto)]),
                 #Luz = sum(Importe[Codigo=="174" & grepl("Naturgy", Concepto)]) +
                 #      sum(Importe[Codigo=="174" & grepl("Gesternova", Concepto)])
                )
SaldosMes
SaldosMes %>% print(n=nrow(SaldosMes))

mean(SaldosMes$Ingresos)
mean(SaldosMes$Gastos)
max(SaldosMes$Gastos)
min(SaldosMes$Gastos)



SaldosMes %>% summarise(n())                  # ENSEÑA FILAS POR AÑO. ??????
SaldosMes %>% summarise(MaxGasto = max(SaldosMes$Gastos),
                  MedGasto = mean(SaldosMes$Gastos),
                  MinGasto = min(SaldosMes$Gastos)
                 )




