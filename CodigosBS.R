source("LOAD_BS.R")

# Códigos, Filtrados los códigos realmente existentes en Movs,
# Cargadas desde el Excel sus descripciones, y
# Ordenados por número de Codigo
Cods <- as_tibble_col(unique(BS$Codigo), column_name = "Codigo")
Cods

Codigos <-  Cods %>% 
            left_join(unique(read_excel("data/Cods.xlsx")), by = "Codigo") %>% 
            arrange(Codigo)
Codigos
rm(Cods)

ggplot(BS, aes(Descripcion)) + geom_bar() + coord_flip()

PorCodigo <- BS %>% 
  group_by(Codigo, Descripcion) %>%
  summarize(num = n(), total = sum(Importe), absoluto = abs(total), media = mean(Importe)) %>%
  arrange(desc(num))
PorCodigo

ggplot(BS) + geom_bar(aes(Descripcion)) + coord_flip()
ggplot(PorCodigo) + geom_bar(aes(x = Descripcion, y = num), stat ="identity") + coord_flip()
ggplot(PorCodigo) + geom_bar(aes(x = reorder(Descripcion, num), y = num), stat ="identity") + coord_flip()
ggplot(PorCodigo) + geom_bar(aes(x = reorder(Descripcion, -num), y = num), stat ="identity") + coord_flip()

#      https://r-graph-gallery.com/267-reorder-a-variable-in-ggplot2.html
## Library
#library(ggplot2)
#library(dplyr)
#
## Dataset 1: one value per group
#data <- data.frame(
#  name=c("north","south","south-east","north-west","south-west","north-east","west","east"),
#  val=sample(seq(1,10), 8 )
#)
#
#data %>%
#  arrange(val) %>%    # First sort by val. This sort the dataframe but NOT the factor levels
#  mutate(name=factor(name, levels=name)) %>%   # This trick update the factor levels
#  ggplot( aes(x=name, y=val)) +
#  geom_segment( aes(xend=name, yend=0)) +
#  geom_point( size=4, color="orange") +
#  coord_flip() +
#  theme_bw() +
#  xlab("")
#
#data %>%
#  arrange(val) %>%
#  mutate(name = factor(name, levels=c("north", "north-east", "east", "south-east", "south", "south-west", "west", "north-west"))) %>%
#  ggplot( aes(x=name, y=val)) +
#  geom_segment( aes(xend=name, yend=0)) +
#  geom_point( size=4, color="orange") +
#  theme_bw() +
#  xlab("")
#
#


ggplot(PorCodigo) + geom_bar(aes(x = Descripcion, y = total), stat ="identity") + coord_flip()
ggplot(PorCodigo) + geom_bar(aes(x = reorder(Descripcion, total), y = total), stat ="identity") + coord_flip()
ggplot(PorCodigo) + geom_bar(aes(x = reorder(Descripcion, absoluto), y = absoluto), stat ="identity") + coord_flip()

ggplot(PorCodigo) + geom_bar(aes(x = Descripcion, y = num), stat ="identity") + coord_flip()
ggplot(PorCodigo) + geom_bar(aes(x = reorder(Descripcion, num), y = num), stat ="identity") + coord_flip()
ggplot(PorCodigo) + geom_bar(aes(x = reorder(Descripcion, -num), y = num), stat ="identity") + coord_flip()

ggplot(PorCodigo) + geom_bar(aes(x = Descripcion, y = media), stat ="identity") + coord_flip()
ggplot(PorCodigo) + geom_bar(aes(x = reorder(Descripcion, media), y = media), stat ="identity") + coord_flip()
ggplot(PorCodigo) + geom_bar(aes(x = reorder(Descripcion, absoluto), y = absoluto), stat ="identity") + coord_flip()

