library(writexl)
source("LOAD_ING.R")
source("FLAG_ING.R")

#===============================================================================
#     AÑADE AL MOVIMIENTO Fecha_Final DE PERIODO 
#     Para agrupar los datos en graficos o cuadros-resumen.

# Aunque el periodo de reporting (por mes, trimestre, ...) es parte del REPORT
# la Fecha_Final se añade en FLAG para poderla utilizar en gráficos de barras
# acumulados por Categorías

#  por meses
FLAG <- FLAG %>%
        mutate(Fecha_Final=as_date(ceiling_date(Fecha, unit = "month")-1))  

#  cambiando month por quarter para trimestres  
#FLAG <- FLAG %>%
#        mutate(Fecha_Final=as_date(ceiling_date(Fecha, unit = "quarter")-1))  

#===============================================================================
#    AÑADIR AL MOVIMIENTO CATEGORIAS AGREGADAS ( O DESAGREGADAS ) 
#    ESPECÍFICAS PARA INFORMES Y GRÁFICOS

FLAG <- FLAG %>% mutate(
        Total_Gasto = Casa | Recibo_Cte | Gasto_Cte | Recibo_Otr | Gasto_Otr,
        Total_sin_Casa = Recibo_Cte | Gasto_Cte | Recibo_Otr | Gasto_Otr

#    Recibos = Comunidad | Telefono | Luz ,
#    Gastos = Servicio | Recibos | Gasto_Cte | Gasto_Otro ,
#    Comunidad = (Categoria == "Recibos")&
#                (grepl("CARGO DE RECIBOS", Descripcion)&
#                 (grepl("Recibo Cp Rfv", Concepto)|
#                  grepl("Recibo Raimundo Fernandez Villaverde", Concepto)|
#                  grepl("Recibo Geminis I, Garaje", Concepto),
#    Telefono=(Categoria == "Recibos")&
#             (grepl("CARGO DE RECIBOS", Descripcion)&
#             grepl("Recibo Yoigo", Concepto)
#             )
                        )  
#==============================================================================
#     TOTALIZANDO IMPORTES POR FECHA_FINAL 
#     Y POR CATEGORIA DESDE VARIABLES FILTRO,
#     O CALCULANDO NUEVOS VALORES (p.e. TOTALES O SUBTOTALES DESDE MOVIMIENTOS)

REPORT <- FLAG
REPORT <- REPORT %>% 
# filter(Fecha_Final > "2022-09-30",
#        Fecha_Final =< "2025-03-31") %>%  # Elegir Fecha de Inicio y/o Final
# filter(Total_Gasto)
  arrange(Fecha_Final) %>%                 # Asegurar el orden por fechas
  group_by(Fecha_Final) %>%           # Calcular Totales por Fecha y Categoria 
  summarise(Numero=n(),               # (lo que elijas definir en el "summarise" 
            Suma=sum(Importe),        #  para gráficos o cuadros-resumen )
            Cobros=sum(Importe[Importe>0]),
            Pagos=sum(Importe[Importe<0]),
            Nomina=sum(Importe[Nomina]),
            Casa=sum(Importe[Casa]),
            Recibo_Cte=sum(Importe[Recibo_Cte]),
            Gasto_Cte=sum(Importe[Gasto_Cte]),
            Recibo_Otr=sum(Importe[Recibo_Otr]),
            Gasto_Otr=sum(Importe[Gasto_Otr]),
            Total_Gasto=sum(Importe[Total_Gasto]),            
#            Otro_Total_Gasto=Casa+Recibo_Cte+Gasto_Cte+Recibo_Otr+Gasto_Otr,
            Total_sin_Casa=sum(Importe[Total_sin_Casa]),
#            Otro_Total_sin_Casa=Recibo_Cte+Gasto_Cte+Recibo_Otr+Gasto_Otr,
            Patrimonio=sum(Importe[Patrimonio]),                 
            # Promedia entre movimientos dentro del periodo
            #avGasto_Cte=mean(Importe[Gasto_Cte]), 
            #sdGasto_Cte=sd(Importe[Gasto_Cte]),
            Check=sum(Importe[Check]),
            OtroCheck= near( Suma,
                             (Nomina + 
                                Casa +
                                Recibo_Cte +
                                Gasto_Cte +
                                Recibo_Otr +
                                Gasto_Otr +
                                Patrimonio
                             )
            )
  ) #  %>% 
#  select(Fecha_Final, Casa, Recibo_Cte, Recibo_Otr, Gasto_Otr, Patrimonio)
#  select(Fecha_Final, Casa:Total_sin_Casa)

#===============================================================================
#       CALCULANDO VALORES A PARTIR DE LOS TOTALES, AÑADIRLOS A REPORT

REPORT$Gasto_Cte
( Anterior <- lag(REPORT$Gasto_Cte) )


length(REPORT$Total_Gasto)
sum(REPORT$Total_Gasto)
mean(REPORT$Total_Gasto) # Promedia los totales de todos los periodos
sd(REPORT$Total_Gasto)   # Desviación típica entre los del periodo

length(REPORT$Gasto_Cte)
sum(REPORT$Gasto_Cte)
mean(REPORT$Gasto_Cte) # Promedia los totales de todos los periodos
sd(REPORT$Gasto_Cte)   # Desviación típica entre los del periodo

length(REPORT$Gasto_Otr)
sum(REPORT$Gasto_Otr)
mean(REPORT$Gasto_Otr)  # Promedia los totales de todos los periodos
sd(REPORT$Gasto_Otr)    # Desviación típica entre los del periodo


# Define funciones media movil de los 3 ó 4 últimos periodos
MM3 <- function(MiVar){
  (MiVar+lag(MiVar, n=1)+lag(MiVar, n=2))/3
} # Promedia totales de los 3 últimos periodos (el ultimo trimestre, para meses)

MM4 <- function(MiVar){
  (MiVar+lag(MiVar, n=1)+lag(MiVar, n=2)+lag(MiVar, n=3))/4
} # Promedia totales de los 4 últimos periodos (el ultimo año, para trimestres)

# Ejemplo uso MM3
MM3(REPORT$Gasto_Cte) # Promedia los totales de los 3 últimos periodos


#summary(REPORT$Gasto_Cte)
summary(abs(REPORT$Gasto_Cte))
length(abs(REPORT$Gasto_Cte))
sum(REPORT$Gasto_Cte)
mean(abs(REPORT$Gasto_Cte)) # Promedia los totales de todos los periodos
sd(abs(REPORT$Gasto_Cte))
max(abs(REPORT$Gasto_Cte))
min(abs(REPORT$Gasto_Cte))
median(abs(REPORT$Gasto_Cte))
quantile(abs(REPORT$Gasto_Cte), prob=0.25)
quantile(abs(REPORT$Gasto_Cte), prob=0.75)
quantile(abs(REPORT$Gasto_Cte), prob=0.00)
quantile(abs(REPORT$Gasto_Cte), prob=1.00)
MM3(abs(REPORT$Total_Gasto))

#summary(REPORT$Total_Gasto)
summary(abs(REPORT$Total_Gasto))
length(abs(REPORT$Total_Gasto))
sum(abs(REPORT$Total_Gasto))
mean(abs(REPORT$Total_Gasto)) # Promedia los totales de todos los periodos
sd(abs(REPORT$Total_Gasto))
max(abs(REPORT$Total_Gasto))
min(abs(REPORT$Total_Gasto))
median(abs(REPORT$Total_Gasto))
quantile(abs(REPORT$Total_Gasto), prob=0.25)
quantile(abs(REPORT$Total_Gasto), prob=0.75)
quantile(abs(REPORT$Total_Gasto), prob=0.00)
quantile(abs(REPORT$Total_Gasto), prob=1.00)
MM3(abs(REPORT$Total_Gasto))


# Añadiendo campos calculados para un cálculo ad-hoc
REPORT %>% mutate(G_Cte_lag=lag(Gasto_Cte),
                   MEAN=mean(REPORT$Gasto_Cte),
                   MiMM3=MM3(Gasto_Cte),
                   MiMM4=MM4(Gasto_Cte)
) %>% 
  select(Fecha_Final, Gasto_Cte, G_Cte_lag, MEAN, MiMM3, MiMM4) %>% 
  print(n=nrow(REPORT))

# # Añadiendo campos calculados al propio REPORT
REPORT <- REPORT %>% mutate(G_Cte_MM3=MM3(Gasto_Cte))

REPORT <- REPORT %>% mutate(G_Cte_MM3=MM3(Gasto_Cte),
                            Total_Gasto_MM3=MM3(Total_Gasto)
                            )



#===============================================================================
#          TOTALIZANDO IMPORTES POR CATEGORIA DESDE VARIABLE CATEGORÍA
#          (HACE LO MISMO QUE PARA CONSTRUIR REPORT, Y EL RESULTADO ES IGUAL)


#         # Podría servir si se quiere eliminar las variables Filtro del Dataset
#  
#  REPORT2 <- REPORT %>%          
#    #    # ¿ Quizá filtrando antes, para gráficos y cuadros-resumen ad-hoc
#    #    #  Hace falta también adaptar el "summarise"
#    #
#    #       filter(Categoria=="Comunidad") %>% 
#    #       filter(Categoria=="Comunidad"|Categoria=="Telefono") %>% 
#    #       filter(Categoria=="Comunidad"|Categoria=="Telefono"|Categoria=="Transferencias") %>% 
#    #       filter(abs(Importe)>=200) %>% 
#    
#    arrange(Fecha_Final) %>%                 # Asegurar el orden por fechas
#    group_by(Fecha_Final) %>%                # Calcular Totales por Fecha y Categoria 
#    summarise(Numero=n(),                    # (lo que elijas definir en el "summarise")
#              Suma=sum(Importe),
#              Cobros=sum(Importe[Importe>0]),
#              Pagos=sum(Importe[Importe<0]),
#              Transferencias=sum(Importe[Categoria == "Transferencias"]),
#              Servicio=sum(Importe[Categoria == "Servicio"]),
#              Comunidad=sum(Importe[Categoria == "Comunidad"]),
#              Telefono=sum(Importe[Categoria == "Telefono"]),
#              Luz=sum(Importe[Categoria =="Luz"]),
#              Gasto_Cte=sum(Importe[Categoria == "Gasto_Cte"]),
#              Gasto_Otro=sum(Importe[Categoria == "Gasto_Otro"]),
#              Check=sum(Importe[Categoria == "Check"]),
#              Recibos=Comunidad + Telefono + Luz,
#              Gastos=Servicio + Recibos + Gasto_Cte + Gasto_Otro
#             )          
#              
#=================================================================================                                                              
#                    TRASPONER ESTE DATA FRAME

TO_EXCEL <- pivot_longer(REPORT,cols = (-Fecha_Final), names_to="Categoria") %>% 
  pivot_wider(names_from=c(Fecha_Final))

# PARA EXPORTAR A EXCEL
write_xlsx(TO_EXCEL, 'data/TO_EXCEL.xlsx')

#=================================================================================                                                              
#    GRAFICOS SOBRE REPORT

ggplot(REPORT) +                      # GRÁFICO LINEAS POR MESES CON INGRESOS, GASTOS 
  #  geom_line(aes(Fecha_Final, abs(Transferencias)), colour="RED", linetype = "dotted") +
  geom_line(aes(Fecha_Final, abs(Nomina)), colour="BLUE",linetype = "dotted") +
  #  geom_line(aes(Fecha_Final, abs(Gastos)-abs(Gasto_Otro)), colour="RED",linetype = "dotdash") +
  geom_line(aes(Fecha_Final, abs(Casa)), colour="GREEN") +
  geom_line(aes(Fecha_Final, abs(Recibo_Cte)), colour="YELLOW") +
  geom_line(aes(Fecha_Final, abs(Recibo_Otr)), colour="BLACK",linetype = "dotdash") +
  geom_line(aes(Fecha_Final, abs(Gasto_Cte)), colour="BLUE") +
  geom_line(aes(Fecha_Final, abs(Gasto_Otr)), colour="RED") +
  geom_line(aes(Fecha_Final, (abs(Casa)+
                              abs(Recibo_Cte)+
                              abs(Recibo_Otr)+
                              abs(Gasto_Cte)+
                              abs(Gasto_Otr)
                             )
               ), colour="DARKGREY"
            )


# ggplot(REPORT) +                      # GRÁFICO PUNTOS POR MESES CON INGRESOS, GASTOS 
#   #  geom_point(aes(Fecha_Final, abs(Transferencias)), colour="RED") +
#   geom_point(aes(Fecha_Final, abs(Nomina)), colour="BLUE", shape=11) +
#   geom_point(aes(Fecha_Final, abs(Casa)), colour="DARKGREY") +
#   geom_point(aes(Fecha_Final, abs(Recibo_Cte)), colour="YELLOW") +
#   geom_point(aes(Fecha_Final, abs(Recibo_Otr)), colour="BLACK") +
#   geom_point(aes(Fecha_Final, abs(Gasto_Cte)), colour="BLUE") +
#   geom_point(aes(Fecha_Final, abs(Gasto_Otr)), colour="RED")
# 
# #  
# #                                      # Barras Superpuestas desde 0, no stacked
# #                                      # GRÁFICO BARRAS POR MESES CON INGRESOS, GASTOS 
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

#===============================================================================

#  NO VA, NECESITA FECHA_FINAL, que no está ahora en Report ni en FLAG
#  HAY QUE REESTRUCTURAR, LOS GRAFICOS POR CATEGORIAS EN BARRAS
#. NECESITAN ARRANCAR DESDE LOS MOVIMIENTOS, NO DESDE SUS TOTALES ????
XXX <- FLAG
XXX <- XXX %>% filter(!Nomina,!Patrimonio)
XXX <- XXX %>% group_by(Fecha_Final, Categoria) %>% 
               summarise(Suma=sum(Importe))

#   Eliminando además lo de casa
#  XXX <- REPORT
#  XXX <- XXX %>% filter(!Nomina,!Patrimonio,!Casa)
#  XXX <- XXX %>% group_by(Fecha_Final, Categoria) %>% 
#    summarise(Suma=sum(Importe))


ggplot(XXX, aes(x=Fecha_Final, y=-Suma, fill=Categoria)) +
  geom_col()

ggplot(XXX, aes(x=Fecha_Final, y=-Suma, fill=Categoria)) +
  geom_col(position="dodge")



ggplot(XXX, aes(x=Fecha_Final, y=-Suma, fill=Categoria)) +
  geom_col()+
  geom_point(aes(x=Fecha_Final, y=Nomina),
             data=REPORT,
             inherit.aes = FALSE)+  
  geom_segment(aes(x = Fecha_Final, y = 0, xend = Fecha_Final, yend = Nomina),
               data = REPORT,
               inherit.aes = FALSE)


ggplot(XXX, aes(x=Fecha_Final, y=-Suma, fill=Categoria)) +
  geom_col(position="dodge")+
  geom_point(aes(x=Fecha_Final, y=Nomina),
             data=REPORT,
             inherit.aes = FALSE)+  
  geom_segment(aes(x = Fecha_Final, y = 0, xend = Fecha_Final, yend = Nomina),
               data = REPORT,
               inherit.aes = FALSE)


#===============================================================================

# GRAFICOS SOBRE FLAG CON BARRAS APILADAS POR CATEGORIA
# Sin filtrar antes


# Desde FLAG Compensación de Gasto sale como ingreso
# Desde FLAG no netea importes por Categoría,
# sino pinta dobles movimientos + y -
# de la misma Categoría (p.e. en Gasto_Cte)
# geom_bar()

#    ggplot(FLAG, aes(x=Fecha_Final, y=Importe, fill=Categoria)) +
#      geom_bar(position='stack', stat='identity')
#    
#    ggplot(FLAG, aes(x=Fecha_Final, y=Importe, fill=Categoria)) +
#      geom_bar(position='dodge', stat='identity')

# geom_col()  por default es position = "stack"
ggplot(FLAG, aes(x=Fecha_Final, y=Importe, fill=Categoria)) +
  geom_col()

#   ggplot(FLAG, aes(x=Fecha_Final, y=Importe, fill=Categoria)) +
#     geom_col(position='stack')

ggplot(FLAG, aes(x=Fecha_Final, y=Importe, fill=Categoria)) +
  geom_col(position='dodge')


# Desde GROUPED_FLAG netea importes + y - en cada Categoría
# antes de Abrir la suma total neta por Categorías

# Totalizo Importes por Fecha_Final y Categoria
# Para graficos stack y dodge (con totales netos por categoria)
GROUPED_FLAG <- FLAG %>% filter(Categoria=="Gasto_Cte") %>%
  group_by(Fecha_Final, Categoria) %>%
  summarise(Suma=sum(Importe))

ggplot(GROUPED_FLAG, aes(x=Fecha_Final, y=-Suma, fill=Categoria)) +
  geom_col()

ggplot(GROUPED_FLAG, aes(x=Fecha_Final, y=abs(Suma), fill=Categoria)) +
  geom_col(position='dodge')

ggplot(GROUPED_FLAG, aes(x=Fecha_Final, y=-Suma, colour=Categoria)) +
  #  geom_col(fill="LIGHTGREY")+
  geom_line()+
  geom_point()

# DISTINTOS GEOMS CON DISTINTO ORIGEN DE DATOS
ggplot(GROUPED_FLAG, aes(x=Fecha_Final, y=-Suma, fill=Categoria)) +
  geom_col()+
  geom_point(aes(x=Fecha_Final, y=Nomina), data=REPORT, inherit.aes = FALSE)


ggplot()+
  geom_col(aes(x=Fecha_Final, y=-Suma, fill=Categoria), data=GROUPED_FLAG)+
  geom_point(aes(x=Fecha_Final, y=Nomina), data=REPORT)+
  geom_line(aes(x=Fecha_Final, y=Nomina), data=REPORT, colour="DARKGREY")+
  geom_segment(aes(x = Fecha_Final, y = 0, xend = Fecha_Final, yend = Nomina),
               data = REPORT,
               inherit.aes = FALSE)#esta linea sobra porque no hereda nada
                                   #de la linea inicial de ggplot

ggplot()+
  geom_col(aes(x=Fecha_Final, y=-Suma, fill=Categoria), data=GROUPED_FLAG)+
  #  geom_point(aes(x=Fecha_Final, y=-G_Cte_MM3), data=REPORT)+
  #  geom_line(aes(x=Fecha_Final, y=-G_Cte_MM3), data=REPORT, colour="YELLOW")+
  geom_segment(aes(x = Fecha_Final, y = 0, xend = Fecha_Final, yend = -G_Cte_MM3),
               data = REPORT,
               inherit.aes = FALSE)#esta linea sobra porque no hereda nada
                                   #de la linea inicial de ggplot

#================================================================================

# Ahora filtrando antes
DDD <- FLAG
DDD <- FLAG %>% filter(Categoria=="Comunidad")
DDD <- FLAG %>% filter(Categoria=="Comunidad"|Categoria=="Telefono")
DDD <- FLAG %>% filter(Categoria=="Comunidad"|Categoria=="Telefono"|Categoria=="Transferencias")
DDD <- FLAG %>% filter(Importe<0|Categoria=="Otros")   # Serviria para solo incluir pagos
DDD <- FLAG %>% filter(abs(Importe)>=200) # Serviria para separar Cte de Otr en Tarjetas, Transf, etc

DDD <- FLAG %>% filter(Recibos)
DDD <- FLAG %>% filter(Gastos)

#  ggplot(DDD, aes(x=Fecha_Final, y=Importe, fill=Categoria)) +
#    geom_bar(position='stack', stat='identity')
#  
#  ggplot(DDD, aes(x=Fecha_Final, y=Importe, fill=Categoria)) +
#    geom_bar(position='dodge', stat='identity')

ggplot(DDD, aes(x=Fecha_Final, y=-Importe, fill=Categoria)) +
  geom_col()

# Va mal mezclando ingresos y gastos
ggplot(DDD, aes(x=Fecha_Final, y=abs(Importe), fill=Categoria)) +
  geom_col()

ggplot(DDD, aes(x=Fecha_Final, y=Importe, fill=Categoria)) +
  geom_col(position='stack')

ggplot(DDD, aes(x=Fecha_Final, y=abs(Importe), fill=Categoria)) +
  geom_col(position='dodge')



# Son todas las Categorías, por su orden default
Cat=sort(unique(FLAG$Categoria))
Cat
# Define orden desde abajo hacia arriba en el stack
# y de derecha a izquierda en el dodge
# Ordénalas para el grafico

Cat_levels=c("Telefono","Comunidad","Otros","Cajero","Luz","Transferencias","NC")
#Rev_Cat_levels=rev(Cat_levels)        # Invierte el orden, si hace falta

length(Cat)==length(Cat_levels)     # CHECK HAY TANTOS NIVELES COMO CATEGORIAS


#============================================================================
# ORDENAR LAS CATEGORIAS EN LOS GRAFICOS APILADOS

# En DDD por orden de Cat_levels para el grafico
# Si quieres, las filtras antes, p.e. filter(Importe<0,!Categoria=="NC")
DDD <- FLAG %>% filter(Gastos) %>%    
  mutate(Cat_Ord = factor(Categoria, levels = rev(Cat_levels)) ) 

ggplot(DDD, aes(x=Fecha_Final, y=abs(Importe), fill=Cat_Ord)) +
  geom_bar(position='stack', stat='identity')

ggplot(DDD, aes(x=Fecha_Final, y=abs(Importe), fill=Cat_Ord)) +
  geom_col(position='stack')

ggplot(DDD, aes(x=Fecha_Final, y=abs(Importe), fill=Cat_Ord)) +
  geom_col() + 
  geom_line(y=abs(Importe))

ggplot(DDD, aes(x=Fecha_Final, y=abs(Importe), fill=Cat_Ord)) +
  geom_col(position="dodge")
