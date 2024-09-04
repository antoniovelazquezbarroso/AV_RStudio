library(tidyverse)
library(readxl)



library(lubridate)
today()
now()



Movs <- read_excel("data/Movs.xlsx") # Movimientos 01JUL2022-31JUL2024
                                     # Por fecha operación orden inverso

Unicods <- unique(read_excel("data/Cods.xlsx")) # Elimina los repetidos, 
                                                # se usa para crear Cods

#AllCods <- read_excel("data/Cods.xlsx").    # Todos los del banco
#Unicods <- unique(AllCods)                  # Elimina los repetidos
print(Unicods %>% arrange(Codigo), n=nrow(Unicods))

# Filtrados los códigos realmente existentes en Movs, y
# Cargadas desde el Excel sus descripciones, y
# Ordenados por número de Codigo
OtherCods <- as_tibble_col(unique(Movs$Codigo), column_name = "Codigo") %>%
  left_join(unique(read_excel("data/Cods.xlsx")), by = "Codigo") %>% 
  arrange(Codigo)



# A partir de Movs y Unicods
Cods <- as_tibble_col(unique(Movs$Codigo), column_name = "Codigo") %>% 
          left_join(Unicods, by = "Codigo") %>% 
          arrange(Codigo)


Cods # Solo codigos existentes en Movs, con Descripcion, ordenados por Codigo
dim(Cods)
names(Cods)
str(Cods)
summary(Cods)
unique(Cods$Codigo)
length(unique(Cods$Codigo))


# Empezando desde los códigos
MovsByCods <- Cods %>% 
            left_join(Movs, by = "Codigo") %>%
            group_by(Codigo, Descripcion) %>% 
            summarise(n(),
                      sum(Importe),
                      mean(Importe),
                      max(abs(Importe)),
                      min(abs(Importe),
                      first(Importe),
                      sum(Importe)/n(),
                      )
MovsByCods
#Por desarrollar Importe movimiento medio
#Por desarrollar Importe mensual medio

MovsByCodsByMonth <- Cods %>% 
  left_join(Movs, by = "Codigo") %>%
  group_by(Codigo, Descripcion, Año, Mes) %>%
  summarise(n(),
            sum(Importe),
            mean(Importe)
  )

print(MovsByCodsByMonth, n=nrow(MovsByCodsByMonth))



#Otro similar a MovsByCods, empezando por los movimientos
MovsWithCods <- Movs %>% 
                  left_join(Cods, by = "Codigo") %>% 
                  select(Dia, Mes, Año, Codigo, Descripcion, Importe, Concepto, FechaNS) %>% 
                  arrange(FechaNS) %>% 
                  group_by(Codigo, Descripcion, Año, Mes) %>% 
                  summarise(numero = n(),
                            total_mes = sum(Importe),
                            )

MovsWithCods
print(MovsWithCods, n=nrow(MovsWithCods))










MonthlyMovsWithCods <-  MovsWithCods %>% 
            group_by(Codigo, Descripcion) %>% 
            summarise(meses = n(),media_mes=mean(total_mes))
MonthlyMovsWithCods



Movs
dim(Movs)
names(Movs)
str(Movs)
summary(Movs)
unique(Movs$Codigo)                 # Utilizado (arriba) para construir Cods
length(unique(Movs$Codigo))
Cods                      # Ampliando con Descripcion y Ordenando por Codigo
MovsByCods

ggplot(Movs, aes(Importe)) +         # Histograma de movimientos por Importe
  geom_histogram(bins = 500)

ggplot(Movs, aes(FechaNS, Saldo)) +         # Histograma de movimientos por Importe
  geom_line()




Movs %>%
  left_join(Unicods, by = "Codigo") %>%
  arrange(desc(Importe))

Movs %>% filter(Codigo=="174") %>% arrange(FechaNS) %>% print(n=102)

Movs %>% filter(Codigo=="174") %>% group_by("Concepto") %>% summarise(n()) 

Movs %>% filter(Codigo=="174", grepl("Geminis", Concepto)) %>% arrange(FechaNS)


Recibos <- Movs %>%
            filter(Codigo=="174")
unique(Recibos$Concepto)

dim(Recibos)
count(Recibos)








( AMPLIADA <- Movs %>%
                group_by(Codigo) %>% 
                summarise(n())
)

(L <- AMPLIADA %>% 
        left_join(Unicods, by = "Codigo")
)


(LL <- Movs %>% 
#       group_by(Codigo) %>% 
        summarise(n(),
#                  count(Movs$Mes==4),
                  sum(Importe),
                  max(Importe),
                  mean(Importe),
                  min(Importe)
                )
)

LL %>% 
  left_join(Unicods, by = "Codigo")

(
RDO <- Movs %>% 
       group_by(Codigo, Año, Mes) %>% 
       summarise(n(),
                 total = sum(Importe),
                 medio = sum(Importe)/n(),
                 )
)



RDO %>% 
  left_join(Unicods, by = "Codigo")



names(RDO) 

R2 <- RDO

R2 %>% 
  group_by(Codigo) %>% 
  summarise(n(),
            sum(total),
  )

Movs %>% 
  group_by(Codigo, Año, Mes) %>% 
  summarise(numero = n(),
            total = sum(Importe),
            promedio = total/numero
            )


LL <- Movs %>% 
  group_by(Codigo) %>% 
  summarise(n(),
            sum(Importe)
            )

LL %>% 
  left_join(Unicods, by = "Codigo")


(MIO <- Movs %>% 
  group_by(Año, Mes, Codigo) %>% 
  summarise(n(),
            uno = sum(Importe),
            dos = (Importe)/25,
           )
)

MIO %>% 
  group_by(Codigo) %>% 
  summarise(mean(uno)) %>% 
  left_join(Unicods, by = "Codigo")


names(MIO)

names(MIO)
names(Unicods)

MIO %>%
  left_join(Unicods, by = "Codigo") %>% 
  select(Año, Mes, uno, Codigo,Descripcion) %>% 
  group_by(Codigo) %>% 
  summarise(n(),
            mean(uno)
           )

Movs %>% 
  left_join(unique(Cods), by = "Codigo") %>% 
  select(Año, Mes, Dia, Importe, Codigo, Concepto, Descripcion, FechaNS) %>% 
  arrange(FechaNS) %>% 
  group_by(Año, Mes, Codigo) %>% 
  summarise( n(),
             sum(Importe)
           )

Movs %>% 
  left_join(unique(Cods), by = "Codigo") %>% 
  select(Año, Mes, Dia, Importe, Codigo, Concepto, Descripcion, FechaNS) %>% 
  arrange(FechaNS) %>% 
  group_by(Codigo) %>% 
  summarise( n(),
             sum(Importe)
  )





(TRANS = Movs %>% 
          select(Año, Mes, Dia, Importe, Codigo, Concepto, FechaNS) %>% 
          arrange(FechaNS) %>% 
          group_by(Año, Mes, Codigo) %>% 
          summarise(n(),
                    sum(Importe)
                    )
                     
)                     



YYY = TRANS %>% 
      left_join(unique(Cods), by = "Codigo")

(ZZZ= YYY %>%
      group_by(Año) %>% 
      summarise(n(),
                mean(Importe)
)
  

Movs %>% 
  select(Año, Mes, Dia, Importe, Codigo, Concepto) %>% 
  inner_join(Cods, by = "Codigo")


sum(Movs$Importe)                   # Total VarSaldo
sum(Movs$Importe[Movs$Importe>0]) # Total Cobros
sum(Movs$Importe[Movs$Importe<0]) # Total Pagos


Movs %>% 
  group_by(Año, Mes) %>% 
  summarise(n(),
            sum(Movs$Importe))


Movis <- Movs
Movis %>% count()
Movis %>% filter(Codigo==174) %>% count()


Movs %>% 
  group_by(Año, Mes) %>% 
  summarise(n(),
            sum(Movs$Importe),                   # Total VarSaldo
            sum(Movs$Importe[Movs$Importe>0]), # Total Cobros
            sum(Movs$Importe[Movs$Importe<0]) # Total Pagos
           )

Movs %>% 
  group_by(Año, Mes, Codigo) %>% 
  summarise(n(),
            sum(Importe),
            sum(Importe[Importe>0]),
            sum(Importe[Importe<0]),
            sum(Importe[Codigo=="071"]),
            mean(Importe)
  )




Movs %>% 
  filter(Importe>0) %>% 
  group_by(Año, Mes, Codigo) %>% 
  summarise(n(),
            sum(Importe),
            sum(Importe[Importe>100]),
            sum(Importe[Codigo=="135"]),
            mean(Importe)
            )





Movis2 <- Movs
print(n=25, Movis2 %>%
  group_by(Año, Mes) %>%
  summarise(n(),
            total = sum(Movis2$Importe),
            cobros = sum(Movis2$Importe[Movis2$Importe>0]),
            pagos = sum(Importe[Movis2$Importe<0])
            )
      )


Movis3 <- Movs
print(n=40, Movis3 %>%
        filter(Importe>0) %>% 
        group_by(Año, Mes, Codigo) %>%
        summarise(n(), total = sum(Importe))
)

Movis4 <- Movs
print(n=40, Movis4 %>%
            left_join(CODS, by = "Codigo")
    )

