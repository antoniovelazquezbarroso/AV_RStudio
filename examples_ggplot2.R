
# R FOR DATA SCIENCE, GGPLOT2 EXAMPLES

library(tidyverse)     # CONTIENE EL DATASET mpg

# =  GEOM_POINT y GEOM_SMOOTH ==================================================

# NUBE DE PUNTOS HIGHWAY MPG SEGÚN CILINDRADA
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy)) 

# NUBE DE PUNTOS ESCRITA MÁS BREVE
ggplot(mpg) +
  geom_point(aes(displ, hwy))

# NUBE DE PUNTOS ESCRITA MÁS BREVE Y CON MAPPING GENERAL
ggplot(mpg,aes(displ, hwy)) +
  geom_point()

# NUBE DE PUNTOS CON jitter
ggplot(data = mpg, aes(x = displ, y = hwy)) +
  geom_jitter() 


# SMOOTH PREDICTION CON "loess" Y SE (default)
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_smooth()

# MÁS BREVE
ggplot(mpg,aes(displ, hwy)) +
  geom_smooth()

# SMOOTH PREDICTION CON "lm" Y SIN SE
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_smooth(method='lm', se = FALSE)

# LOS PUNTOS Y PREDICCIONES SUPERPUESTOS
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point() +
  geom_smooth()


#    FACETING

# LOS PUNTOS EN GRÁFICOS INDEPENDIENTES PARA CADA CATEGORÍA
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point() +
  facet_wrap( ~class , nrow=2)

# GRÁFICOS INDEPENDIENTES SEGÚN TRACCIÓN Y NUM. CILINDROS
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point() +
  facet_grid(drv ~ cyl)


# LOS PUNTOS COLOREADOS SEGÚN CATEGORÍA
ggplot(data = mpg, mapping = aes(x = displ, y = hwy, colour= class)) +
  geom_point()

# LOS PUNTOS COLOREADOS SEGÚN TRACCIÓN
ggplot(data = mpg, mapping = aes(x = displ, y = hwy, colour=drv)) +
  geom_point()

# DISTINTAS FORMAS DE PUNTO SEGÚN TRACCIÓN
ggplot(data = mpg, mapping = aes(x = displ, y = hwy, shape=drv)) +
  geom_point()

# DISTINTAS FORMAS COLOREADAS SEGÚN TRACCIÓN
ggplot(data = mpg, mapping = aes(x = displ, y = hwy, shape=drv,colour=drv)) +
  geom_point()

# 20 DISTINTAS FORMAS DE PUNTO
plot(1:20,1:20,pch=1:20)

# SCATTERPLOT pch=9
ggplot(mpg) +
  geom_point(aes(displ, hwy), pch=9)

# 4 MANERAS DE PINTAR EL PUNTO, SEGÚN CATEGORÍA
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, color = class))

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, size = class))

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, alpha = class))

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, shape = class))

# CAMBIANDO EL COLOR DE TODOS LOS PUNTOS DEL geom_point
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, size = class), colour="red")

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, alpha = class), colour="blue", size=7)

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, shape = class), colour="green")


# DISTINTAS FORMAS COLOREADAS SEGÚN TRACCIÓN EN GRÁFICOS PARA CADA CATEGORÍA
ggplot(data = mpg, mapping = aes(x = displ, y = hwy, shape=drv,colour=drv)) +
  geom_point() +
  facet_wrap( ~class , nrow=2)

# PUNTOS COLOREADOS SEGÚN TRACCIÓN, CON PREDICCIONES SUPERPUESTAS
ggplot(data = mpg, mapping = aes(x = displ, y = hwy, colour=drv)) +
  geom_point() +
  geom_smooth()

# LO MISMO, CON LÍNEAS DIFERENTES, SIN SE Y SIN LEYENDA
ggplot(data = mpg, mapping = aes(x = displ, y = hwy, linetype=drv, colour=drv)) +
  geom_smooth(show.legend = FALSE, se = FALSE) +
  geom_point(show.legend = FALSE)

# LÍNEAS DIFERENTES SEGÚN TRACCIÓN, Y COLORES SEGÚN CATEGORÍA
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_smooth(mapping=aes(linetype = drv)) +
  geom_point(mapping = aes(colour = class))

# LO MISMO, PERO SIN SE NI LEYENDAS
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_smooth(mapping=aes(linetype = drv), se = FALSE, show.legend = FALSE) +
  geom_point(mapping = aes(colour = class), show.legend = FALSE)

# DISTINTAS FORMAS SEGÚN CLASE, COLOREADAS SEGÚN TRACCIÓN
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_smooth(mapping=aes(colour = drv)) +
  geom_point(mapping = aes(shape = class, colour = drv))


# =  GEOM_BAR ==============================================================

# Default geom_bar recuento por variable categóríca
ggplot(mpg,aes(x=trans)) + geom_bar()

# Explicitando que el default stat es el recuento (count)
ggplot(mpg,aes(x=trans, y=after_stat(count))) + geom_bar()

# Calculando la proporción (prop) (ojo al agrupamiento)
ggplot(mpg,aes(x=trans, y=after_stat(prop), group = 1)) + geom_bar()


# GRÁFICO DE BARRAS (CUENTA/FRECUENCIA SEGÚN cut)
ggplot(data = diamonds) + geom_bar(mapping = aes(x = cut))

# Más breve
ggplot(diamonds, aes(x = cut)) + 
  geom_bar()

# LO MISMO COLOREADO (COMO ADORNO)
ggplot(data = diamonds) + geom_bar(mapping = aes(x = cut, fill=cut))

# RECUENTO SEGÚN cut Y clarity POR COLORES STACKED PARA CADA cut
ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut, fill=clarity))

# Explicitando que el default de position es stack
ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut, fill=clarity), position = "stack")

# RECUENTO SEGÚN cut Y clarity POR COLORES DODGED PARA CADA cut (COMPARAR RECUENTOS)
ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut, fill=clarity), position= "dodge")

# RECUENTO SEGÚN cut Y clarity POR COLORES FILLED PARA CADA cut (COMPARAR PROPORCIONES)
ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut, fill=clarity), position= "fill")


# = GEOM_HISTOGRAM y GEOM_FREQPOLY==============================================

# Default geom_histogram frecuencias (recuento) de variable numérica por tramos
ggplot(mpg, aes(displ)) +
  geom_histogram()

# Explicitando que el default  es recuento
ggplot(mpg, aes(displ)) +
  geom_histogram(aes(y = after_stat(count)))

# Calculando la proporción (densidad)
ggplot(mpg, aes(displ)) +
  geom_histogram(aes(y = after_stat(density)))

# HISTOGRAMA DEFAULT (bins=30)
ggplot(data = diamonds) +
  geom_histogram(mapping = aes(x = price))

# HISTOGRAMA AJUSTANDO bins (SU NÚMERO TOTAL)
ggplot(data = diamonds) +
  geom_histogram(mapping = aes(x = price), bins = 60)

# HISTOGRAMA AJUSTANDO binwidth (EN LAS UNIDADES DE x, AQUÍ price)
ggplot(data = diamonds) +
  geom_histogram(mapping = aes(x = price), binwidth = 200)

# DISTINGUIENDO POR COLORES STACKED SEGÚN cut
ggplot(data = diamonds) +
  geom_histogram(mapping = aes(x = price, fill = cut), binwidth = 200)


# POLÍGONO DE FRECUENCIAS
ggplot(data = diamonds) +
  geom_freqpoly(mapping = aes(x = price), binwidth = 200)

# LO MISMO SIN AJUSTAR bins
ggplot(mpg, aes(displ)) +
  geom_freqpoly()

# DISTINGUIENDO POR COLORES STACKED SEGÚN cut
ggplot(data = diamonds) +
geom_freqpoly(mapping = aes(x = price, colour = cut), binwidth = 200)



# ==   GEOM_BOXPLOT  ======================================================

# BOXPLOT DE depth SEGÚN cut
ggplot(data = diamonds) +
  geom_boxplot(
    mapping = aes(x = cut, y = depth)
  )

# BOXPLOT DE depth SEGÚN cut SIN MOSTRAR OUTLIERS 
ggplot(data = diamonds, mapping = aes(x = cut, y = depth)) +
  geom_boxplot(outlier.shape = NA)


# BOXPLOT DISTINGUIENDO ADEMÁS POR OTRA VARIABLE CUALITATIVA ('dodged')
ggplot(data = diamonds, mapping = aes(x = cut, y = depth, colour=clarity)) +
  geom_boxplot()

# BOXPLOT HORIZONTAL Y CON MUCHAS ETIQUETAS
ggplot(data = mpg, mapping = aes(x = class, y = hwy)) +
  geom_boxplot() +
  coord_flip() +
  labs(y = "Highway MPG",
       x = "Class",
       title = "Highway MPG by car class",
       subtitle = "1999-2008",
       caption = "Source: http://fueleconomy.gov")
