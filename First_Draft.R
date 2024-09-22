library(tidyverse)
library(lubridate)
library(readxl)

# Movimientos 01SEP2022-31AGO2024 por fecha operación orden inverso
NewMovs <- read_excel("data/20220901_20240831.xlsx") 

NewMovs                                                     

#      # Comprobación de integridad del fichero por saldo
#      SaldoInicial <- first(NewMovs$Saldo)
#      SaldoFinal <- last(NewMovs$Saldo)
#      Cobros <- NewMovs %>% 
#        filter(Importe>0) %>% 
#        summarise(sum(Importe))
#      Pagos <-  NewMovs %>% 
#        filter(Importe<0) %>% 
#        summarise(sum(Importe)) 
#      
#      CheckSaldo <- SaldoInicial
#      -SaldoFinal
#      + Cobros
#      - Pagos
#      CheckSaldo == first(NewMovs$Saldo) # Debe ser TRUE
#      rm(SaldoInicial, SaldoFinal,Cobros, Pagos, CheckSaldo)

# Elimina columnas innecesarias, cambia nombres de columnas incómodos
CleanMovs <- NewMovs %>% select(-`Fecha Valor`,-`Divisa...5`,-`Divisa...7`)  
names(CleanMovs)[1] = "Fecha"
names(CleanMovs)[5] = "Codigo"

CleanMovs
rm(NewMovs)

# Convierte a tipo Date la columna Fecha,
# incluye NumOrden de movimientos del banco,
# añade la Descripcion del Codigo
# ordena por Fecha y NumOrden
ChangedMovs <- CleanMovs %>% 
  mutate(
    Fecha = parse_date(Fecha, "%d/%m/%Y"),
    #TextoFecha = format(Fecha, "%d/%m/%Y"),
    NumOrden = (dim(CleanMovs)[1] - row_number( ) + 1)#,
    #MonthDay = mday(Fecha),
    #Month = month(Fecha),
    #Year = year(Fecha)
  ) %>% 
  left_join(unique(read_excel("data/Cods.xlsx")), by = "Codigo") %>% 
  arrange(Fecha, NumOrden)%>% 
  select(Fecha, NumOrden, Importe, Saldo, Codigo, Descripcion, Concepto)

ChangedMovs

Movs <- ChangedMovs   # Cortito
rm(CleanMovs, ChangedMovs)

#      # Códigos, Filtrados los códigos realmente existentes en Movs,
#      # Cargadas desde el Excel sus descripciones, y
#      # Ordenados por número de Codigo
#      Cods <- as_tibble_col(unique(Movs$Codigo), column_name = "Codigo")
#      Cods
#      
#      Codigos <-  Cods %>% 
#        left_join(unique(read_excel("data/Cods.xlsx")), by = "Codigo") %>% 
#        arrange(Codigo) %>% 
#        select(Codigo, Descripcion)
#      Codigos
#      rm(Cods)

ggplot(Movs, aes(Importe)) +         # Histograma de movimientos por Importe
  geom_histogram(bins = 500)

ggplot(Movs, aes(Fecha, Saldo)) +    # Historico de saldos por Fecha
  geom_line()

Movs %>% arrange(desc(Importe))      # Los 10 mayores cobros
Movs %>% arrange(Importe)            # Los 10 mayores pagos

# Movs %>% arrange(Fecha)         # arrange innecesario, Movs ya tiene ese orden


# RECIBOS
Movs %>% filter(Codigo=="174") %>% summarise(n())
count(Movs %>% filter(Codigo=="174"))
Movs %>% filter(Codigo=="174")
Movs %>% filter(Codigo=="174") %>% print(n=nrow(Movs %>% filter(Codigo=="174")))
                               # siguiente arrange innecesario, Movs ya tiene ese orden
# Movs %>% filter(Codigo=="174") %>% arrange(Fecha) %>% print(n=nrow(Movs %>% filter(Codigo=="174")))

Movs %>% filter(Codigo=="174", grepl("Geminis", Concepto)) %>% arrange(Fecha) %>% print(n=nrow(Movs))
Movs %>% filter(Codigo=="174", grepl("Cp Rfv 44", Concepto)) %>% arrange(Fecha) %>% print(n=nrow(Movs))
Movs %>% filter(Codigo=="174", grepl("Yoigo", Concepto)) %>% arrange(Fecha) %>% print(n=nrow(Movs))
Movs %>% filter(Codigo=="174", grepl("Naturgy", Concepto)) %>% arrange(Fecha) %>% print(n=nrow(Movs))
Movs %>% filter(Codigo=="174", grepl("Gesternova", Concepto)) %>% arrange(Fecha) %>% print(n=nrow(Movs))
Movs %>% filter(Codigo=="174", grepl("Recibo Raimundo Fernandez", Concepto)) %>% arrange(Fecha) %>% print(n=nrow(Movs))

Movs %>% filter(Codigo=="174", !grepl("Gesternova", Concepto), # No hay ningún otro recibo
                               !grepl("Naturgy", Concepto),
                               !grepl("Yoigo", Concepto),
                               !grepl("Cp Rfv 44", Concepto),
                               !grepl("Geminis", Concepto),
                               !grepl("Recibo Raimundo Fernandez", Concepto)
                ) %>% arrange(Fecha) %>% print(n=nrow(Movs))

Movs %>% filter(Importe> 200) %>%  print(n=nrow(Movs))


Movs %>% filter(Codigo=="072") %>% arrange(Fecha) %>% print(n=nrow(Movs %>% filter(Codigo=="072")))

#  TARJETAS
Movs %>% filter(Codigo=="136")                                                     # Todos                                            
Movs %>% filter(Codigo=="136", grepl(174534, Concepto))                            # Tarjeta de Eva 
Movs %>% filter(Codigo=="136", grepl(955303, Concepto))                            # Mi tarjeta
Movs %>% filter(Codigo=="136", !grepl(955303, Concepto), !grepl(174534, Concepto)) # No hay ninguno
(Movs %>% filter(Codigo=="136") %>% summarise(n()))
Movs %>% filter(Codigo=="136", grepl("Reintegro, Atm", Concepto)| grepl("Reint. Cajero", Concepto)) %>% print(n=nrow(Movs)) 


#  Movs de tarjetas por meses
Movs %>% filter(Codigo=="136") %>% group_by(Año=year(Fecha), Mes=month(Fecha)) %>% summarise(n()) %>% print(n=nrow(Movs))

MESES <- Movs %>% 
         group_by(Año=year(Fecha), Mes=month(Fecha)) %>% 
         summarise(Num=n(),
                   Total=sum(Importe),
                   Ingresos=sum(Importe[Importe>0]),
                   Gastos=sum(Importe[Importe<0]),
                   S_Inicial = Saldo[NumOrden == first(NumOrden)]- Importe[NumOrden == first(NumOrden)],
                   S_Final = Saldo[NumOrden == last(NumOrden)]
                  )
MESES %>% print(n=nrow(MESES))
MESES$Num
mean(MESES$Num)

MESES <- Movs %>% 
  group_by(Año=year(Fecha), Mes=month(Fecha)) %>% 
  summarise(Num=n(),
            Total=sum(Importe),
            Ingresos=sum(Importe[Importe>0]),
            Gastos=sum(Importe[Importe<0]),
            S_Inicial = Saldo[NumOrden == first(NumOrden)]- Importe[NumOrden == first(NumOrden)],
            S_Final = Saldo[NumOrden == last(NumOrden)],
            Recibos = sum(Importe[Codigo == "174"]),
            Tarjetas = sum(Importe[Codigo == "136"])
  )
MESES %>% print(n=nrow(MESES))


LAB <- Movs %>%
  group_by(Codigo, Descripcion, year(Fecha), month(Fecha)) %>%
  summarise(n())
LAB

LAB <- Movs %>%
       group_by(Codigo, Descripcion, year(Fecha), month(Fecha)) %>%
       summarise(n()) %>% 
       arrange(desc(`n()`)) 
LAB

LAB <- Movs %>%
       group_by(Codigo, Descripcion, year(Fecha), month(Fecha)) %>%
       summarise(n()) %>% 
       arrange(desc(`year(Fecha)`), desc(`month(Fecha)`))
LAB

LAB <- Movs %>%
       group_by(Codigo, Descripcion, Año=year(Fecha), Mes=month(Fecha)) %>%
       summarise(n()) %>% 
       arrange(Codigo, desc(Año), desc(Mes))
LAB

LAB <- Movs %>%
       group_by(Codigo, Descripcion, Año=year(Fecha), Mes=month(Fecha)) %>%  
       summarise(
                 n(),
#                 Año=year(Fecha),   # Hay múltiples ocurrencias
#                 Mes=month(Fecha),  # Hay múltiples ocurrencias
                 sum(Importe),
                 max(Importe),
                 min(Importe),
                 mean(Importe),
                 (sum(Importe)/n()),
                 first(NumOrden),
                 min(NumOrden),
                 Saldo[NumOrden == first(NumOrden)],
                 Importe[NumOrden == first(NumOrden)],
                 Inicial = Saldo[NumOrden == first(NumOrden)]- Importe[NumOrden == first(NumOrden)],
                 min(Fecha),
                 first(Fecha),
                 max(Fecha),
                 last(Fecha)
                ) 
LAB

LAB <- Movs %>%
  group_by(Año = year(Fecha), Mes = month(Fecha)) %>%  
  summarise(
    n(),
    sum(Importe),
    first(NumOrden),
    min(NumOrden),
    SdoInicial = Saldo[NumOrden == first(NumOrden)]- Importe[NumOrden == first(NumOrden)]
  ) %>% 
  arrange(Año, Mes, desc(`sum(Importe)`))
LAB



LAB <- Movs %>%
  group_by(year(Fecha), month(Fecha), Codigo, Descripcion) %>%  
  summarise(
            n(),
            sum(Importe)
            ) %>% 
  arrange(`year(Fecha)`, `month(Fecha)`, desc(`sum(Importe)`))
LAB

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

print(LAB, n = nrow(LAB))
