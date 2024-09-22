source("ProcesaMovs.R")

ggplot(Movs, aes(Importe)) +         # Histograma de movimientos por Importe
  geom_histogram(bins = 500)

ggplot(Movs, aes(Fecha, Saldo)) +    # Historico de saldos por Fecha
  geom_line()


n_meses <- ceiling(interval(first(Movs$Fecha),last(Movs$Fecha))/dmonths(1))
n_meses

Cobros <- Movs %>% filter(Importe>0)
ggplot(Cobros, aes(Importe)) +         # Histograma de Ingresos por Importe
  geom_histogram(bins = 500)

Cobros %>% arrange(desc(Importe)) %>% print(n=20) # Los 20 mayores
count(Cobros)
sum(Cobros$Importe)

Pagos <- Movs %>% filter(Importe<0)
ggplot(Pagos, aes(abs(Importe))) +         # Histograma de Ingresos por Importe
  geom_histogram(bins = 500)

Pagos %>% arrange(Importe) %>% print(n=20) # Los 20 mayores

Tarjetas <- Movs %>% filter(Codigo=="136")    
count( Tarjetas)
max(abs(Tarjetas$Importe))
Tarjetas %>% select(Importe, Fecha,Concepto) %>% arrange(Importe, Fecha) %>% print(n=20) # Los 20 mayores
Tarjetas %>% filter(abs(Importe) == max(abs(Importe)))
mean(abs(Tarjetas$Importe))
min(abs(Tarjetas$Importe))

Cajeros <-  Movs %>% filter(Codigo=="136", grepl("Reintegro, Atm", Concepto)| grepl("Reint. Cajero", Concepto))
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


Comunidad <- Movs %>% filter(
  Codigo=="174" & grepl("Geminis", Concepto)|
    Codigo=="174" & grepl("Cp Rfv 44", Concepto)|
    Codigo=="174" & grepl("Recibo Raimundo Fernandez", Concepto)
)
Comunidad %>% print(n=nrow(Comunidad))


Comunidad %>% group_by(Año= year(Fecha), Mes=month(Fecha)) %>%  summarize(
  Num = n(),
  Total = sum(Importe),
  Garaje = sum(Importe[grepl("Geminis", Concepto)])
) %>% print(n=nrow(Comunidad))


LAB <- Movs %>%
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


mean(LAB$ingresos)
mean(LAB$gastos)
max(LAB$gastos)
min(LAB$gastos)



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



LAB <- Movs %>%
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
