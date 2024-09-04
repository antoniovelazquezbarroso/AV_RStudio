library(tidyverse)
library(nycflights13)

flights
dim(flights)
names(flights)
str(flights)
summary(flights)
unique(flights$carrier)
length(unique(flights$carrier))


(destinos <- flights)                 # 336.776 líneas, 19 columnas

(destinos <- flights %>%
    count()                           # 1 línea x 1 columna (n) con el número 336.776
)

count(flights)                        # 1 línea x 1 columna con el número 336.776
                                      # Aplica directamente la función summarise

mean(flights$dep_delay, na.rm = TRUE) # 1 línea x 1 columna
                                      # Aplica directamente la función summarise

 
(destinos <- flights %>%
    select(dest)                      # 336.776 líneas, 1 columna (dest)
)

(destinos <- flights %>%
    select(dest, origin, carrier)     # 336.776 líneas, 3 columnas (dest, origin, carrier)
    )                                 # Sin ordenar
print(destinos, n=50)

(destinos <- flights %>%
    select(dest, origin, carrier) %>% 
    arrange(desc(carrier))            # Ordena por 1 columna,  336.776 líneas, 3 columnas
)

(destinos <- flights %>%
    select(dest, origin, carrier) %>% 
    arrange(desc(carrier), dest)      # Ordena por 2 columnas, 336.776 líneas, 3 columnas
)

(destinos <- flights %>%              # 1 línea x 1 columna (n) con el número 336.776
    select(dest)  %>%                 # Si vas a contar todas las filas
    count()                           # el select es innecesario e inutil
)

(destinos <- flights %>%              # 1 línea x 1 columna con el número 336.776
    select(dest, origin)  %>%         # Lo mismo (no importa select ni numero de columnas)
    count()
)

(destinos <- flights %>%              # Columna dest x 336.776  filas
    select(dest) %>%                  # Seleccionar columnas no cambia la cuenta
    group_by(dest)                    # Agrupar por sí solo no hace nada 
)                                     # sin SUMMARIZE, FILTER o MUTATE                               
                                      # (no ordena la presentación)

(destinos <- flights %>%              # Columna dest x 336.776  filas
    select(dest) %>%                    # Seleccionar columnas no cambia la cuenta
    arrange(dest)                       # Necesitas ordenar para verlos agrupados
)

(destinos <- flights %>%             
    select(dest) %>%                  # Seleccionar columnas innecesario, no cambia cuenta
    group_by(dest) %>%                # 105 líneas (tantas como distintos destinos)
    count()                           # Al contar sí lo hace agrupado
)

(destinos <- flights %>%              # Lo mismo, más breve
    group_by(dest) %>% 
    count()
)

(destinos <- flights %>%              # 439 líneas (tantas como combinaciones
    select(dest, origin,carrier)  %>%   # únicas de origen, destino y carrier)
    group_by(carrier, origin, dest) %>% # el select es innecesario e inutil
    count()                           # Al contar, lo hace agrupado, y lo presenta
)                                     # en el orden de la agrupación

(destinos <- flights %>%
    group_by(origin) %>% 
    count()
)

(destinos <- flights %>%
    select(origin)  %>%               # el select es innecesario e inutil
    group_by(origin) %>% 
    count()
)

ggplot(flights,aes(origin)) +
  geom_bar() +
    coord_flip()

ggplot(data = mpg) +
  geom_boxplot(
    mapping = aes(
      x = reorder(class, hwy, FUN = median),
      y = hwy
    )
  )





ggplot(flights) +                     # Ver cómo arreglar el orden
  geom_bar(mapping = (aes(x= dest))) +
  coord_flip()


orden_inverso <- arrange(flights, dest)     # NO SIRVE
ggplot(orden_inverso) +                     # Ver cómo arreglar el orden
  geom_bar(aes(x = reorder(dest))) +
    coord_flip()


(destinos <- flights %>%              # Ver cómo arreglar el orden
  select(dest) %>%
  group_by(dest) %>%
  count() %>%    
  arrange(desc(n())) 
)

(destinos <- flights %>%              # Ver cómo arreglar el orden
    select(dest) %>%
    arrange(dest) %>% 
    group_by(dest) %>%
    count() # %>%    
  #  arrange(desc(n())) 
)




# = FILTER  =====================================================

filter(flights, month == 1, day == 1)
jan1 <- filter(flights, month == 1, day == 1)
(dec25 <- filter(flights, month == 12, day == 25))
(filter(flights, month == 11 | month == 12))
(nov_dec <- filter(flights, month %in% c(11, 12)))
(filter(flights, !(arr_delay > 120 | dep_delay > 120)))
