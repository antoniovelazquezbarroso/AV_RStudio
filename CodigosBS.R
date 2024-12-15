source("ProcesaBS.R")

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

ggplot(PorCodigo) + geom_bar(aes(x = Descripcion, y = total), stat ="identity") + coord_flip()
ggplot(PorCodigo) + geom_bar(aes(x = reorder(Descripcion, total), y = total), stat ="identity") + coord_flip()
ggplot(PorCodigo) + geom_bar(aes(x = reorder(Descripcion, absoluto), y = absoluto), stat ="identity") + coord_flip()

ggplot(PorCodigo) + geom_bar(aes(x = Descripcion, y = num), stat ="identity") + coord_flip()
ggplot(PorCodigo) + geom_bar(aes(x = reorder(Descripcion, num), y = num), stat ="identity") + coord_flip()
ggplot(PorCodigo) + geom_bar(aes(x = reorder(Descripcion, -num), y = num), stat ="identity") + coord_flip()

ggplot(PorCodigo) + geom_bar(aes(x = Descripcion, y = media), stat ="identity") + coord_flip()
ggplot(PorCodigo) + geom_bar(aes(x = reorder(Descripcion, media), y = media), stat ="identity") + coord_flip()
ggplot(PorCodigo) + geom_bar(aes(x = reorder(Descripcion, absoluto), y = absoluto), stat ="identity") + coord_flip()

