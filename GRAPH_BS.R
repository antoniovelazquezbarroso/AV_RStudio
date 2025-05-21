# source("LOAD_BS.R")
# source("FLAG_BS.R")
# source("CHECK_BS.R") # ¿AÑADIRLE UN MENSAJE PARA CASO DE ERRORES?
#   No existe CHECK_BS.R, habrá que crearlo si conviene

source("REPORT_BS.R")
source("MyFunctions.R")
#===============================================================================                                                              
#    GRAFICOS SOBRE REPORT

ggplot(REPORT) +                # GRÁFICO LINEAS POR MESES CON INGRESOS, GASTOS 
  #geom_line(aes(Fecha_Final, abs(Transferencias)), colour="DARKGREY", linetype = "dotted") +
  geom_line(aes(Fecha_Final, abs(Total_Gasto)), colour="BLACK") +
  geom_line(aes(Fecha_Final, abs(Total_Fijo)), colour="BLACK",linetype = "dotdash") +
  geom_line(aes(Fecha_Final, abs(Recibos)), colour="YELLOW") +
  geom_line(aes(Fecha_Final, abs(Servicio)), colour="GREEN") +
  geom_line(aes(Fecha_Final, abs(Gasto_Corriente)), colour="BLUE") +
  geom_line(aes(Fecha_Final, abs(Gasto_Otro)), colour="RED") 

ggplot(REPORT) +              # GRÁFICO LINEAS POR MESES CON GASTOS Y SUS MEDIAS
  geom_line(aes(Fecha_Final, abs(Total_Gasto)), colour="DARKGREY") +
  geom_line(aes(Fecha_Final, abs(Total_Fijo)), colour="BLACK") +
  geom_line(aes(Fecha_Final, abs(Recibos)), colour="YELLOW") +
  geom_line(aes(Fecha_Final, abs(Servicio)), colour="GREEN") +  
  geom_line(aes(Fecha_Final, abs(Gasto_Corriente)), colour="BLUE") +
  geom_line(aes(Fecha_Final, abs(Gasto_Otro)), colour="RED") +
  geom_line(aes(Fecha_Final, mean(abs(Total_Gasto))), colour="DARKGREY",linetype = "dotdash") +
  geom_line(aes(Fecha_Final, mean(abs(Total_Fijo))), colour="BLACK",linetype = "dotdash") +
  geom_line(aes(Fecha_Final, mean(abs(Recibos))), colour="YELLOW",linetype = "dotdash") +
  geom_line(aes(Fecha_Final, mean(abs(Servicio))), colour="GREEN",linetype = "dotdash") +
  geom_line(aes(Fecha_Final, mean(abs(Gasto_Corriente))), colour="BLUE",linetype = "dotdash") +  
  geom_line(aes(Fecha_Final, mean(abs(Gasto_Otro))), colour="RED",linetype = "dotdash")

ggplot(REPORT) +                      # GRÁFICO LINEAS RECIBOS DESAGREGADO
  #  geom_line(aes(Fecha_Final, abs(Transferencias)), colour="RED", linetype = "dotted") +
  #  geom_line(aes(Fecha_Final, abs(Gastos)), colour="BLUE",linetype = "dotted") +
  #  geom_line(aes(Fecha_Final, abs(Gasto_Corriente)), colour="GREEN") +
  #  geom_line(aes(Fecha_Final, abs(Gasto_Otro)), colour="ORANGE")+
  #  geom_line(aes(Fecha_Final, abs(Servicio)), colour="DARKGREY")+
  geom_line(aes(Fecha_Final, abs(Recibos)), colour="BLACK",linetype = "dotdash") +
  geom_line(aes(Fecha_Final, abs(Comunidad)), colour="PINK") +
  geom_line(aes(Fecha_Final, abs(Telefono)), colour="RED") +
  geom_line(aes(Fecha_Final, abs(Luz)), colour="GREEN") 

ggplot(REPORT) +            # GRÁFICO LINEAS RECIBOS DESAGREGADO, ANALISIS COMUNIDAD
  #  geom_line(aes(Fecha_Final, abs(Transferencias)), colour="RED", linetype = "dotted") +
  #  geom_line(aes(Fecha_Final, abs(Gastos)), colour="BLUE",linetype = "dotted") +
  #  geom_line(aes(Fecha_Final, abs(Gasto_Corriente)), colour="GREEN") +
  #  geom_line(aes(Fecha_Final, abs(Gasto_Otro)), colour="ORANGE")+
  #  geom_line(aes(Fecha_Final, abs(Servicio)), colour="DARKGREY")+
  geom_line(aes(Fecha_Final, abs(Recibos)), colour="BLACK",linetype = "dotdash") +
  geom_line(aes(Fecha_Final, abs(Comunidad)), colour="LIGHTBLUE") +
  #geom_line(aes(Fecha_Final, abs(Telefono)), colour="RED") +
  #geom_line(aes(Fecha_Final, abs(Luz)), colour="YELLOW")+
  geom_line(aes(Fecha_Final, abs(MM3(Comunidad))), colour="GREEN")+
  geom_line(aes(Fecha_Final, abs(mean(Comunidad))), colour="DARKORANGE",linetype = "dotdash")

#  DESDE REPORT SOLO BARRAS PARA UNA COLUMNA,
#  PUEDES SUPERPONER OTRAS COLUMNAS ( p.e. con alpha ), PERO SIN APILARLAS
#  PUEDES SUPERPONER TAMBIEN LINEAS O PUNTOS

# Solo una columna
ggplot(REPORT, aes(x=Fecha_Final, y= abs(Total_Gasto))) +
  geom_col(fill="NAVYBLUE") 

# Puedes superponer varias, pero no apilarlas
ggplot(REPORT) +
  geom_col(aes(x=Fecha_Final, y= abs(Total_Gasto)), fill="LIGHTGREY") +
  geom_col(aes(x=Fecha_Final, y= abs(Total_Fijo)), fill="RED", alpha=0.8)

# Superpones lineas, o puntos, de cualquier otra columna de REPORT
ggplot(REPORT, aes(x=Fecha_Final, y= abs(Total_Fijo))) +
  geom_col(fill="GREY") +
  geom_line(aes(x=Fecha_Final, y=abs(Total_Gasto)), colour="RED") +
  geom_line(aes(x=Fecha_Final, y=mean(abs(Total_Fijo))), colour="BLUE")+
  geom_line(aes(x=Fecha_Final, y=mean(abs(Total_Fijo))+sd(Total_Fijo)), colour="BLUE",linetype = "dotdash")+
  geom_line(aes(x=Fecha_Final, y=mean(abs(Total_Fijo))-sd(Total_Fijo)), colour="BLUE",linetype = "dotdash")

# Añades campos calculados sobre columnas de REPORT
# para Superponer lineas, o puntos, desde cualquier otra columna de REPORT

#   source("MyFunctions.R")
ADHOC <- REPORT %>% mutate(#G_Cte_MM3=MM3(Gasto_Cte),
                            MM3_Total_Gasto=MM3(Total_Gasto),
                            #Pct_Fijo_Total=REPORT$Total_Fijo/REPORT$Total_Gasto*100
                           ) %>% 
                #     filter(Fecha_Final>= "2023-12-31") %>% 
                    select(Fecha_Final,
                           Total_Gasto,
                           #Pct_Fijo_Total,
                           MM3_Total_Gasto
                          )

ggplot(ADHOC, aes(x=Fecha_Final, y= abs(Total_Gasto))) +
  geom_col(fill="GREY")+
  geom_line(aes(y=abs(MM3_Total_Gasto)),color = "BLUE",linetype = "dotdash") +
  geom_line(aes(y=abs(mean(Total_Gasto))),color = "RED",linetype = "dotdash") 

#===============================================================================

#   LOS GRAFICOS DE BARRAS, DESAGREGADOS POR CATEGORIAS
#   NECESITAN ARRANCAR DESDE LOS MOVIMIENTOS (FLAG), NO DESDE TOTALES (REPORT)

# Total_Gasto Sin Ordenar
# DESDE FLAG PUEDES DIBUJAR COLUMNAS CON TOTALES ABIERTOS POR CATEGORÍAS
GROUPED_FLAGS <- FLAG %>% filter(Total_Gasto) %>% 
                          group_by(Fecha_Final, Categoria) %>% 
                          summarise(Suma=sum(Importe), .groups = "drop")

ggplot(GROUPED_FLAGS, aes(x=Fecha_Final, y=-Suma, fill=Categoria)) +
  geom_col()

ggplot(GROUPED_FLAGS, aes(x=Fecha_Final, y=-Suma, fill=Categoria)) +
  geom_col(position="dodge")

# PUEDES ORDENAR LAS BARRAS COMO QUIERAS 

# Recibos sin Ordenar
# Por default el campo categoría es texto, y lo ordena alfabéticamente
GROUPED_FLAGS <- FLAG %>% filter(Recibos) %>% 
  group_by(Fecha_Final, Categoria) %>% 
  summarise(Suma=sum(Importe), .groups = "drop")
                                            # ME SIRVE EN ESTE ORDEN
ggplot(GROUPED_FLAGS, aes(x=Fecha_Final, y=-Suma, fill=Categoria)) +
  geom_col()

ggplot(GROUPED_FLAGS, aes(x=Fecha_Final, y=-Suma, fill=Categoria)) +
  geom_col(position="dodge")

# Recibos Ordenados
# Transforma el campo categoría a factor ordenado, y lo ordena segun sus levels
# El orden de los levels lo fijas al crear el factor (contraintuitivo, con rev)
# Van de abajo arriba en barras stacked, de derecha a izquierda en barras dodged
GROUPED_FLAGS <- FLAG %>% filter(Recibos) %>% 
  mutate(Categoria_ORD = factor(Categoria, levels= rev(c("Comunidad","Telefono", "Luz" )))) %>% 
  group_by(Fecha_Final, Categoria_ORD, Categoria) %>% # No es necesario group_by Categoria,
  summarise(Suma=sum(Importe), .groups = "drop")      # Lo he dejado para poder después
                                                      # comparar los dos órdenes
#(Cat_levels=sort(unique(GROUPED_FLAGS$Categoria_ORD)))      # ORDENADOS
ggplot(GROUPED_FLAGS, aes(x=Fecha_Final, y=-Suma, fill=Categoria_ORD)) +
  geom_col()
                                                             
ggplot(GROUPED_FLAGS, aes(x=Fecha_Final, y=-Suma, fill=Categoria_ORD)) +
  geom_col(position="dodge")
                                                              # SIN ORDENAR
#(Cat_levels=sort(unique(GROUPED_FLAGS$Categoria)))
ggplot(GROUPED_FLAGS, aes(x=Fecha_Final, y=-Suma, fill=Categoria)) +
  geom_col()

# Total_Gasto Ordenado
GROUPED_FLAGS <- FLAG %>% filter(Total_Gasto) %>% 
                 mutate(Categoria_ORD = factor(Categoria,
                                               levels= rev(c("Luz",
                                                              "Telefono",
                                                              "Comunidad",
                                                              "Servicio",
                                                              "Gasto_Corriente",
                                                              "Gasto_Otro"
                                                             )
                                                           )
                                               )
                        )%>% 
                 group_by(Fecha_Final, Categoria_ORD) %>% 
                 summarise(Suma=sum(Importe), .groups = "drop")

ggplot(GROUPED_FLAGS, aes(x=Fecha_Final, y=-Suma, fill=Categoria_ORD)) +
  geom_col()                                                                    

ggplot(GROUPED_FLAGS, aes(x=Fecha_Final, y=-Suma, fill=Categoria_ORD)) +
  geom_col(position="dodge")

#================================================================
#      ENRIQUECES REPORT CON LO QUE NECESITES GRAFICAR

ADHOC <- REPORT %>% mutate(MEAN=mean(REPORT$Total_Gasto),
                           MiMM3=MM3(Total_Gasto),
                          # MiMM4=MM4(Total_Gasto),
                           MiMM12=MMnum(Total_Gasto, 12)
                          ) %>% 
                    select(Fecha_Final, Total_Gasto, MEAN, MiMM3, MiMM12) %>% 
                    filter(Fecha_Final>"2022-12-31") %>%
                    arrange(rev(Fecha_Final)) %>% 
                    print(n=nrow(REPORT))
ggplot(ADHOC, aes(Fecha_Final, -Total_Gasto)) +
  geom_col(fill="LIGHTGREEN")+
  geom_line(aes(Fecha_Final, -MEAN), colour="BLUE")+
  geom_line(aes(Fecha_Final, -MiMM3), colour="RED")+
  geom_line(aes(Fecha_Final, -MiMM12), colour="YELLOW")

#=====================

# Superpones  a las barras lineas, o puntos; de cualquier otra columna de REPORT

GROUPED_FLAGS <- GROUPED_FLAGS %>% filter(Fecha_Final>"2022-12-31") # Igual que ADHOC
ggplot(GROUPED_FLAGS, aes(x=Fecha_Final, y=-Suma, fill=Categoria_ORD)) +
  geom_col()+
  #  geom_point(aes(x=Fecha_Final, y=-Total_Gasto),
  #             data=ADHOC,
  #             inherit.aes = FALSE, colour="NAVYBLUE")+  
  #  geom_segment(aes(x = Fecha_Final, y = 0, xend = Fecha_Final, yend = -Total_Gasto),
  #               data = REPORT,
  #               inherit.aes = FALSE)+
  geom_line(aes(x=Fecha_Final, y=-MiMM3),
            data=ADHOC,
            inherit.aes = FALSE, colour="BLUE",linetype = "dotdash")+
  geom_line(aes(x=Fecha_Final, y=-MiMM12),
            data=ADHOC,
            inherit.aes = FALSE, colour="DARKORANGE")+
  geom_line(aes(x=Fecha_Final, y=-mean(Total_Gasto)),
            data=ADHOC,
            inherit.aes = FALSE, colour="BLACK",linetype = "dashed")


ggplot(GROUPED_FLAGS, aes(x=Fecha_Final, y=-Suma, fill=Categoria_ORD)) +
  geom_col(position="dodge")


# Total_Fijo en Barras Ordenado y con Medias
#        DESDE FLAG PUEDES DIBUJAR COLUMNAS CON TOTALES ABIERTOS POR CATEGORÍAS
GROUPED_FLAGS <- FLAG %>% filter(Total_Fijo) %>% 
                 mutate(Categoria_ORD = factor(Categoria,
                                               levels= rev(c("Luz",
                                                             "Telefono",
                                                             "Comunidad",
                                                             "Servicio",
                                                             "Gasto_Corriente"
                                                             )
                                                           )
                                              )
                       )%>%
                 group_by(Fecha_Final, Categoria_ORD) %>%
                 filter(Fecha_Final>"2022-12-31") %>%
                 summarise(Suma=sum(Importe), .groups = "drop")
#       ENRIQUECES REPORT CON LO QUE NECESITES GRAFICAR
ADHOC <- REPORT %>% mutate(MEAN_TF=mean(REPORT$Total_Fijo),
                           MiMM3_TF=MM3(Total_Fijo),
                           # MiMM4_TF=MM4(Total_Fijo),
                           MiMM12_TF=MMnum(Total_Fijo, 12),
                           MEAN_TG=mean(REPORT$Total_Gasto),
                           MiMM3_TG=MM3(Total_Gasto),
                           # MiMM4_TG=MM4(Total_Gasto),
                           MiMM12_TG=MMnum(Total_Gasto, 12),
                          ) %>% 
                    select(Fecha_Final,
                           MEAN_TF,
                           MiMM3_TF,
                           # MiMM4_TF,
                           MiMM12_TF,
                           Total_Gasto,                           
                           MEAN_TG,
                           MiMM3_TG,
                           # MiMM4_TG,
                           MiMM12_TG                           
                          ) %>% 
                    filter(Fecha_Final>"2022-12-31") %>%
                    arrange(rev(Fecha_Final))   %>% 
                    print(n=nrow(REPORT))

ggplot(GROUPED_FLAGS, aes(x=Fecha_Final, y=-Suma, fill=Categoria_ORD)) +
  geom_col()

ggplot()+
  geom_col(aes(x=Fecha_Final, y=-Suma, fill=Categoria_ORD), data=GROUPED_FLAGS)+
  geom_line(aes(x=Fecha_Final, y=-MEAN_TF), data=ADHOC, colour="BLACK")+
  geom_line(aes(x=Fecha_Final, y=-MiMM3_TF), data=ADHOC, colour="BLACK",linetype = "dashed")+
  geom_line(aes(x=Fecha_Final, y=-MiMM12_TF), data=ADHOC, colour="BLACK",linetype = "dotted")+
  geom_line(aes(x=Fecha_Final, y=-Total_Gasto), data=ADHOC, colour="VIOLET")+  
  geom_line(aes(x=Fecha_Final, y=-MEAN_TG), data=ADHOC, colour="VIOLET")+
  geom_line(aes(x=Fecha_Final, y=-MiMM3_TG), data=ADHOC, colour="VIOLET",linetype = "dashed")+
  geom_line(aes(x=Fecha_Final, y=-MiMM12_TG), data=ADHOC, colour="VIOLET",linetype = "dotted") #+

ggplot(data=ADHOC, aes(x=Fecha_Final))+
  geom_line(aes(y=-MEAN_TF),      colour="BLACK")+
  geom_line(aes(y=-MiMM3_TF),     colour="BLACK",linetype = "dashed")+
  geom_line(aes(y=-MiMM12_TF),    colour="BLACK",linetype = "dotted")+
  geom_point(aes(y=-Total_Gasto), colour="VIOLET")+
  geom_segment(aes(y= 0, xend = Fecha_Final, yend = -Total_Gasto), colour="VIOLET")+
  geom_line(aes(y=-MEAN_TG),    colour="VIOLET")+
  geom_line(aes(y=-MiMM3_TG),   colour="VIOLET",linetype = "dashed")+
  geom_line(aes(y=-MiMM12_TG),  colour="VIOLET",linetype = "dotted") +
  geom_col(aes(x=Fecha_Final, y=-Suma, fill=Categoria_ORD, alpha=0.9), data=GROUPED_FLAGS)


#===============================================================================

# DISTINTOS GEOMS CON DISTINTO ORIGEN DE DATOS

ggplot()+
  geom_col(aes(x=Fecha_Final, y=-Suma, fill=Categoria_ORD), data=GROUPED_FLAGS)+
#  geom_point(aes(x=Fecha_Final, y=-MiMM3), data=ADHOC)+
  geom_line(aes(x=Fecha_Final, y=-MiMM3), data=ADHOC, colour="BLUE",linetype = "dotdash")+
  geom_line(aes(x=Fecha_Final, y=-mean(Total_Gasto)), data=ADHOC, colour="YELLOW") #+
#  geom_segment(aes(x = Fecha_Final, y = 0, xend = Fecha_Final, yend = -MiMM3),
#               data = ADHOC,
#               inherit.aes = FALSE)#esta linea sobra porque no hereda nada
                                   #de la linea inicial de ggplot

#================================================================================

# ANTIGUOS BORRADORES

# ggplot(REPORT) +               # GRÁFICO PUNTOS POR MESES CON INGRESOS, GASTOS 
#   #  geom_point(aes(Fecha_Final, abs(Transferencias)), colour="RED") +
#   geom_point(aes(Fecha_Final, abs(Nomina)), colour="BLUE", shape=11) +
#   geom_point(aes(Fecha_Final, abs(Casa)), colour="DARKGREY") +
#   geom_point(aes(Fecha_Final, abs(Recibo_Cte)), colour="YELLOW") +
#   geom_point(aes(Fecha_Final, abs(Recibo_Otr)), colour="BLACK") +
#   geom_point(aes(Fecha_Final, abs(Gasto_Cte)), colour="BLUE") +
#   geom_point(aes(Fecha_Final, abs(Gasto_Otr)), colour="RED")
# 
# #  
# #                              # Barras Superpuestas desde 0, no stacked
# #                              # GRÁFICO BARRAS POR MESES CON INGRESOS, GASTOS 
# #                                                            # con geom_bar   
# #  ggplot(REPORT) +                       
# #  #  geom_bar(aes(Fecha_Final, abs(Transferencias)), stat="identity", fill="DARKSALMON") +
# #    geom_line(aes(Fecha_Final, Transferencias+Gastos),  colour="BLACK") +
# #    geom_bar(aes(Fecha_Final, abs(Gastos)), stat="identity", fill="DEEPSKYBLUE", alpha=0.2) +
# #    geom_bar(aes(Fecha_Final, abs(Comunidad)), stat="identity", fill="DARKGREY")
# #  
# #                                                            # con geom_col
# #  ggplot(REPORT) +                        
# #  #  geom_col(aes(Fecha_Final, Transferencias), fill="RED", alpha=0.2) +
# #  #  geom_line(aes(Fecha_Final, Transferencias+Gastos), colour="BLACK",linetype = "dotdash") +
# #    geom_col(aes(Fecha_Final, Gastos), fill="BLUE", alpha=0.2) +
# #    geom_col(aes(Fecha_Final, Gasto_Cte), fill="LIGHTGREEN", alpha=0.8) +
# #    geom_col(aes(Fecha_Final, Comunidad), fill="YELLOW")
# #  
# ggplot(REPORT, aes(Fecha_Final, -Gasto_Otr)) +
#   geom_line()            
# 
# ggplot(REPORT, aes(Fecha_Final, -Gasto_Otr)) +
#   geom_point()
# 
# ggplot(REPORT, aes(Fecha_Final, -Gasto_Otr)) +
#   geom_point(shape=11, colour="RED", size=2)+
#   geom_segment(aes(x = Fecha_Final, y = 0, xend = Fecha_Final, yend = -Gasto_Otr))
# 
# ggplot(REPORT)+
#   geom_col(aes(x=Fecha_Final, y=-Gasto_Otr))
# 
# ggplot(REPORT)+
#   geom_col(aes(Fecha_Final,-Gasto_Otr), fill="PINK")
# 
# # Barras y Líneas, Totales y Medias
# ggplot(REPORT,aes(x=Fecha_Final, y=abs(Gasto_Otr)))+
#   geom_col()+
#   geom_line(aes(y=abs(Recibo_Cte)), colour="YELLOW")+
#   geom_line(aes(y=mean(abs(Recibo_Otr))), colour="ORANGE")+
#   #  geom_line(aes(y=-G_Cte_MM3), colour="BLUE")+
#   geom_col(aes(y=abs(Gasto_Cte), fill="PINK"), alpha=0.7)

