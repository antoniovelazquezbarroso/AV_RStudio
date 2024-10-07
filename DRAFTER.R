PorCodigo <- BS %>% 
             group_by(Codigo, Descripcion) %>%
             summarize(num = n(), total = sum(Importe)) %>%
             arrange(desc(num))
PorCodigo

ggplot(PorCodigo) + geom_bar(aes(x = Descripcion, y = total), stat ="identity") + coord_flip()

ggplot(BS, aes(x = Descripcion)) +
  geom_bar() +
  coord_flip()

ggplot(BS, aes(x = Descripcion)) +
  geom_bar(aes(y = after_stat(count))) +
  coord_flip()

ggplot(BS, aes(x = Descripcion)) +
  geom_bar(aes(y = after_stat(prop), group = 1)) +
  coord_flip()







