source("ProcesaBS.R")


#===================================================

ggplot(BS, aes(x = Descripcion)) +    
  geom_bar() +        # Si no pones coord_flip ordena, de izquierda-derecha por orden alfabetico  de descripcion
  coord_flip()        # Si pones coord_flip ordena de abajo-arriba por orden alfabetico  de descripcion

ggplot(BS, aes(x = Descripcion)) +
  geom_bar(aes(y = after_stat(count))) + # El recuento, count, valor por omisión en geom_bar()
  coord_flip()

ggplot(BS, aes(x = Descripcion)) +
  geom_bar(aes(y = after_stat(prop), group = 1)) + #  La proporción
  coord_flip()

ggplot(BS) +     # Creo que no calcula como quiero
  geom_bar(aes(x = Descripcion, y = mean(Importe)), stat = "identity") +
  coord_flip()


# BOXPLOT HORIZONTAL Y CON MUCHAS ETIQUETAS
ggplot(data = mpg, mapping = aes(x = class, y = hwy)) +
  geom_boxplot() +
  coord_flip() +
  labs(y = "Highway MPG",
       x = "Class",
       title = "Highway MPG by car class",
       subtitle = "1999-2008",
       caption = "Source: http://fueleconomy.gov")

# BARCHART HORIZONTAL Y CON MUCHAS ETIQUETAS
ggplot(data = mpg, mapping = aes(x = class, y = hwy)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(y = "Highway MPG",
       x = "Class",
       title = "Highway MPG by car class",
       subtitle = "1999-2008",
       caption = "Source: http://fueleconomy.gov")


# NO ORDENA COMO QUIERO
ggplot(data = mpg, mapping = aes(x = reorder(class, -hwy), y = hwy)) +
  geom_bar(stat = "identity") +
  coord_flip()


mpg %>%
  select(class, hwy) %>%
  group_by(class) %>%
  summarize(n(), mean(hwy)) %>%
  arrange(`mean(hwy)`)  # POR OMISIÓN boxplot reordena según la media

ggplot(data = mpg, mapping = aes(x = reorder(class, -hwy), y = hwy)) +
  geom_boxplot() +
  coord_flip()

ggplot(data = mpg, mapping = aes(x = reorder(class, -hwy, FUN = mean), y = hwy)) +
  geom_boxplot() +
  coord_flip()

ggplot(data = mpg, mapping = aes(x = reorder(class, -hwy, FUN = min), y = hwy)) +
  geom_boxplot() +
  coord_flip()

ggplot(data = mpg, mapping = aes(x = reorder(class, -hwy, FUN = max), y = hwy)) +
  geom_boxplot() +
  coord_flip()

mpg %>%
  select(class, hwy) %>%
  group_by(class) %>%
  summarize(n(), mean(hwy)) %>%
  arrange(`mean(hwy)`)  # POR OMISIÓN boxplot reordena según la media




