#   source("LOAD_ING.R")
#   source("FLAG_ING.R")
#   # source("CHECK_ING.R") # ¿AÑADIRLE UN MENSAJE PARA CASO DE ERRORES?
source("REPORT_ING.R")

#=================================================================================                                                              
#    GRAFICOS SOBRE REPORT

ggplot(REPORT) +                # GRÁFICO LINEAS POR MESES CON INGRESOS, GASTOS 
  #  geom_line(aes(Fecha_Final, abs(Transferencias)), colour="RED", linetype = "dotted") +
  #  geom_line(aes(Fecha_Final, abs(Nomina)), colour="BLUE",linetype = "dotted") +
  #  geom_line(aes(Fecha_Final, abs(Gastos)-abs(Gasto_Otro)), colour="RED",linetype = "dotdash") +
  geom_line(aes(Fecha_Final, abs(Casa)), colour="GREEN") +
  #  geom_line(aes(Fecha_Final, abs(Recibo_Cte)), colour="YELLOW") +
  #  geom_line(aes(Fecha_Final, abs(Recibo_Otr)), colour="BLACK",linetype = "dotdash") +
  geom_line(aes(Fecha_Final, abs(Gasto_Cte)), colour="BLUE") +
  geom_line(aes(Fecha_Final, abs(Recibos)), colour="DARKGREY") +
  geom_line(aes(Fecha_Final, abs(Gasto_Otr)), colour="RED")  +
  geom_line(aes(Fecha_Final, abs(Total_Gasto)), colour="BLACK") # +
#  geom_line(aes(Fecha_Final, (abs(Casa)+
#                              abs(Recibo_Cte)+
#                              abs(Recibo_Otr)+
#                              abs(Gasto_Cte)+
#                              abs(Gasto_Otr)
#                             )
#               ), colour="DARKGREY"
#            )

ggplot(REPORT) +              # GRÁFICO LINEAS POR MESES CON GASTOS Y SUS MEDIAS
  geom_line(aes(Fecha_Final, abs(Total_Gasto)), colour="DARKGREY") +
  geom_line(aes(Fecha_Final, abs(Total_sin_Casa)), colour="BLACK") +
  geom_line(aes(Fecha_Final, abs(Recibos)), colour="GREEN") +
  geom_line(aes(Fecha_Final, abs(Gasto_Cte)), colour="BLUE") +
  geom_line(aes(Fecha_Final, abs(Gasto_Otr)), colour="RED") +
  geom_line(aes(Fecha_Final, mean(abs(Total_Gasto))), colour="DARKGREY",linetype = "dotdash") +
  geom_line(aes(Fecha_Final, mean(abs(Total_sin_Casa))), colour="BLACK",linetype = "dotdash") +
  geom_line(aes(Fecha_Final, mean(abs(Recibos))), colour="GREEN",linetype = "dotdash") +
  geom_line(aes(Fecha_Final, mean(abs(Gasto_Cte))), colour="BLUE",linetype = "dotdash") +  
  geom_line(aes(Fecha_Final, mean(abs(Gasto_Otr))), colour="RED",linetype = "dotdash")
#  Otro Total_Gasto
#  geom_line(aes(Fecha_Final, (abs(Casa)+
#                              abs(Recibo_Cte)+
#                              abs(Recibo_Otr)+
#                              abs(Gasto_Cte)+
#                              abs(Gasto_Otr)
#                             )
#                ), colour="DARKGREY")


#  DESDE REPORT SOLO BARRAS PARA UNA COLUMNA,SUPERPONES OTRAS(CON LINEAS,PUNTOS)
# Solo una columna
ggplot(REPORT, aes(x=Fecha_Final, y= abs(Gasto_Cte))) +
  geom_col(fill="GREY")



# Superpones lineas, o puntos, de cualquier otra columna de REPORT
source("MyFunctions.R")

# Añades campos calculados sobre columnas de REPORT
REPORT <- REPORT %>% mutate(G_Cte_MM3=MM3(Gasto_Cte),
                            Total_Gasto_MM3=MM3(Total_Gasto),
                            Pct_Casa_Total=REPORT$Casa/REPORT$Total_Gasto*100
                           ) %>% 
                #     filter(Fecha_Final>= "2023-12-31") %>% 
                     select(Fecha_Final, Total_Gasto, Total_Gasto_MM3)

ggplot(REPORT, aes(x=Fecha_Final, y= abs(Total_Gasto))) +
  geom_col(fill="GREY")+
  geom_line(aes(y=abs(Total_Gasto_MM3)),color = "BLUE",linetype = "dotdash") +
  geom_line(aes(y=abs(mean(Total_Gasto))),color = "RED",linetype = "dotdash") 

#===============================================================================

#   LOS GRAFICOS DE BARRAS, DESAGREGADOS POR CATEGORIAS
#   NECESITAN ARRANCAR DESDE LOS MOVIMIENTOS (FLAG), NO DESDE TOTALES (REPORT)

# DESDE FLAG PUEDES DIBUJAR COLUMNAS CON TOTALES ABIERTOS POR CATEGORÍAS
XXX <- FLAG %>% filter(Total_Gasto) %>% 
                group_by(Fecha_Final, Categoria) %>% 
                summarise(Suma=sum(Importe))

ggplot(XXX, aes(x=Fecha_Final, y=-Suma, fill=Categoria)) +
  geom_col()

ggplot(XXX, aes(x=Fecha_Final, y=-Suma, fill=Categoria)) +
  geom_col(position="dodge")

# PUEDES ORDENAR LAS BARRAS COMO QUIERAS 

# Recibos Ordenado
# Por default el campo categoría es texto, y lo ordena alfabéticamente
XXX <- FLAG %>% filter(Recibos) %>% 
  group_by(Fecha_Final, Categoria) %>% 
  summarise(Suma=sum(Importe))

ggplot(XXX, aes(x=Fecha_Final, y=-Suma, fill=Categoria)) +
  geom_col()

ggplot(XXX, aes(x=Fecha_Final, y=-Suma, fill=Categoria)) +
  geom_col(position="dodge")

# Transforma el campo categoría a factor ordenado, y lo ordena segun sus levels
# El orden de los levels lo fijas al crear el factor (contraintuitivo, con rev)
# Van de abajo arriba en barras stacked, de derecha a izquierda en barras dodged
XXX <- FLAG %>% filter(Recibos) %>% 
  mutate(Categoria_ORD = factor(Categoria, levels= rev(c("Recibo_Cte", "Recibo_Otr"))))%>% 
  group_by(Fecha_Final, Categoria_ORD, Categoria) %>% # No es necesario group_by Categoria,
  summarise(Suma=sum(Importe))                        # Lo he dejado para poder después
# comparar los dos órdenes
#(Cat_levels=sort(unique(XXX$Categoria_ORD)))
ggplot(XXX, aes(x=Fecha_Final, y=-Suma, fill=Categoria_ORD)) +
  geom_col()

ggplot(XXX, aes(x=Fecha_Final, y=-Suma, fill=Categoria_ORD)) +
  geom_col(position="dodge")

#(Cat_levels=sort(unique(XXX$Categoria)))
ggplot(XXX, aes(x=Fecha_Final, y=-Suma, fill=Categoria)) +
  geom_col()


# Total_Gasto Ordenado


XXX <- FLAG %>% filter(Total_Gasto) %>% 
  mutate(Categoria_ORD = factor(Categoria, levels= rev(c("Recibo_Cte", "Recibo_Otr","Casa","Gasto_Cte", "Gasto_Otr"))))%>% 
  group_by(Fecha_Final, Categoria_ORD, Categoria) %>% 
  summarise(Suma=sum(Importe), .groups = "drop")                       
ggplot(XXX, aes(x=Fecha_Final, y=-Suma, fill=Categoria_ORD)) +
  geom_col()                                                                    

ggplot(XXX, aes(x=Fecha_Final, y=-Suma, fill=Categoria_ORD)) +
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

# Superpones  a las barras lineas, o puntos, de cualquier otra columna de REPORT

XXX <- XXX %>% filter(Fecha_Final>"2022-12-31") # Igual que ADHOC
ggplot(XXX, aes(x=Fecha_Final, y=-Suma, fill=Categoria_ORD)) +
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


ggplot(XXX, aes(x=Fecha_Final, y=-Suma, fill=Categoria_ORD)) +
  geom_col(position="dodge") #+
#  geom_point(aes(x=Fecha_Final, y=Nomina),
#             data=REPORT,
#             inherit.aes = FALSE)+  
#  geom_segment(aes(x = Fecha_Final, y = 0, xend = Fecha_Final, yend = Nomina),
#               data = REPORT,
#               inherit.aes = FALSE)


#===============================================================================

# DISTINTOS GEOMS CON DISTINTO ORIGEN DE DATOS

source("MyFunctions.R")
source("REPORT_ING.R")
REPORT <- REPORT %>% mutate(G_Cte_MM3=MM3(Gasto_Cte),
                            Total_Gasto_MM3=MM3(Total_Gasto),
                            Pct_Casa_Total=REPORT$Casa/REPORT$Total_Gasto*100
)

XXX <- FLAG %>% filter(Total_Gasto) %>% 
                group_by(Fecha_Final, Categoria) %>% 
                summarise(Suma=sum(Importe))

ggplot(XXX, aes(x=Fecha_Final, y=-Suma, fill=Categoria)) +
  geom_col()+
  geom_point(aes(x=Fecha_Final, y=-Recibos), data=REPORT, inherit.aes = FALSE)


ggplot()+
  geom_col(aes(x=Fecha_Final, y=-Suma, fill=Categoria), data=XXX)+
  #  geom_point(aes(x=Fecha_Final, y=-Total_Gasto_MM3), data=REPORT)+
  geom_line(aes(x=Fecha_Final, y=-Total_Gasto_MM3), data=REPORT, colour="BLUE",linetype = "dotdash")+
  geom_line(aes(x=Fecha_Final, y=-mean(Total_Gasto)), data=REPORT, colour="YELLOW") #+
#  geom_segment(aes(x = Fecha_Final, y = 0, xend = Fecha_Final, yend = -Total_Gasto_MM3),
#               data = REPORT,
#               inherit.aes = FALSE)#esta linea sobra porque no hereda nada
#                                   #de la linea inicial de ggplot



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





