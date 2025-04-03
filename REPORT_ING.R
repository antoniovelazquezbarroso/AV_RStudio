library(writexl)
source("LOAD_ING.R")
source("FLAG_ING.R")

#===============================================================================
#    AÑADIR CATEGORIAS AGREGADAS ( O DESAGREGADAS ) PARA INFORMES Y GRÁFICOS

#FLAG <- FLAG %>% mutate(
#Recibos = Comunidad | Telefono | Luz ,
#Gastos = Servicio | Recibos | Gasto_Corriente | Gasto_Otro
#   Comunidad = (Categoria == "Recibos")&
#               (grepl("CARGO DE RECIBOS", Descripcion)&
#                 (grepl("Recibo Cp Rfv", Concepto)|
#                  grepl("Recibo Raimundo Fernandez Villaverde", Concepto)|
#                  grepl("Recibo Geminis I, Garaje", Concepto)
#   Telefono=(Categoria == "Recibos")&
#             (grepl("CARGO DE RECIBOS", Descripcion)&
#               grepl("Recibo Yoigo", Concepto)
#             )
#                         )  

#===============================================================================
#     AÑADE AL MOVIMIENTO Fecha_Final DE PERIODO 
# para agrupar los datos en graficos o cuadros-resumen

#  por meses
FLAG <- FLAG %>% mutate(Fecha_Final=as_date(ceiling_date(Fecha, unit = "month")-1))  

#  cambiando month por quarter para trimestres  
#FLAG <- FLAG %>% mutate(Fecha_Final=as_date(ceiling_date(Fecha, unit = "quarter")-1))  

#===============================================================================
#     TOTALIZANDO IMPORTES POR FECHA_FINAL Y CATEGORIA DESDE VARIABLES FILTRO

REFLAG <- FLAG %>% 
  arrange(Fecha_Final) %>%                 # Asegurar el orden por fechas
  group_by(Fecha_Final) %>%      # Calcular Totales por Fecha y Categoria 
  summarise(Numero=n(),          # (lo que elijas definir en el "summarise" 
            Suma=sum(Importe),   #  para gráficos o cuadros-resumen )
            Cobros=sum(Importe[Importe>0]),
            Pagos=sum(Importe[Importe<0]),
            Nomina=sum(Importe[Nomina]),
            Casa=sum(Importe[Casa]),
            Recibo_Cte=sum(Importe[Recibo_Cte]),
            Gasto_Cte=sum(Importe[Gasto_Cte]),
            Recibo_Otr=sum(Importe[Recibo_Otr]),
            Gasto_Otr=sum(Importe[Gasto_Otr]),
            Patrimonio=sum(Importe[Patrimonio]),                 
            # No sirve, promedia por numero de operaciones dentro del periodo
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
#  select(Fecha_Final, Nomina, Casa, Recibo_Cte, Recibo_Otr, Gasto_Otr, Patrimonio)
#  select(Fecha_Final, Nomina:Patrimonio)

#===============================================================================
#            CALCULANDO VALORES A PARTIR DE LOS TOTALES, AÑADIRLOS A REFLAG

REFLAG$Gasto_Cte
( Anterior <- lag(REFLAG$Gasto_Cte) )
sum(REFLAG$Gasto_Cte)
mean(REFLAG$Gasto_Cte) # Promedia los totales de todos los periodos
sd(REFLAG$Gasto_Cte)
sum(REFLAG$Gasto_Otr)
mean(REFLAG$Gasto_Otr)
sd(REFLAG$Gasto_Otr)

MM3 <- function(MiVar){
  (MiVar+lag(MiVar, n=1)+lag(MiVar, n=2))/3
}
MM3(REFLAG$Gasto_Cte) # Promedia los totales de los 3 últimos periodos

# Añadiendo campos calculados para un cálculo ad-hoc
REFLAG %>% mutate(G_Cte_lag=lag(Gasto_Cte),
                   MEAN=mean(REFLAG$Gasto_Cte),
                   MiMM3=MM3(Gasto_Cte)
) %>% 
  select(Fecha_Final, Gasto_Cte, G_Cte_lag, MEAN, MiMM3) %>% 
  print(n=nrow(REFLAG))

# # Añadiendo campos calculados al propio REFLAG
REFLAG <- REFLAG %>% mutate(G_Cte_MM3=MM3(Gasto_Cte))

#===============================================================================
#          TOTALIZANDO IMPORTES POR CATEGORIA DESDE VARIABLE CATEGORÍA
#          (HACE LO MISMO QUE PARA CONSTRUIR REFLAG, Y EL RESULTADO ES IGUAL)


#         # Podría servir si se quiere eliminar las variables Filtro del Dataset
#  
#  REFLAG2 <- FLAG %>%          
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
#              Gasto_Corriente=sum(Importe[Categoria == "Gasto_Corriente"]),
#              Gasto_Otro=sum(Importe[Categoria == "Gasto_Otro"]),
#              Check=sum(Importe[Categoria == "Check"]),
#              Recibos=Comunidad + Telefono + Luz,
#              Gastos=Servicio + Recibos + Gasto_Corriente + Gasto_Otro
#             )          
#              
#=================================================================================                                                              
#                    TRASPONER ESTE DATA FRAME

BALER <- pivot_longer(REFLAG,cols = (-Fecha_Final), names_to="Categoria") %>% 
  pivot_wider(names_from=c(Fecha_Final))

# PARA EXPORTAR A EXCEL
write_xlsx(BALER, 'data/BALER.xlsx')

#=================================================================================                                                              
#    GRAFICOS SOBRE REFLAG

ggplot(REFLAG) +                      # GRÁFICO LINEAS POR MESES CON INGRESOS, GASTOS 
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


ggplot(REFLAG) +                      # GRÁFICO PUNTOS POR MESES CON INGRESOS, GASTOS 
  #  geom_point(aes(Fecha_Final, abs(Transferencias)), colour="RED") +
  geom_point(aes(Fecha_Final, abs(Nomina)), colour="BLUE", shape=11) +
  geom_point(aes(Fecha_Final, abs(Casa)), colour="DARKGREY") +
  geom_point(aes(Fecha_Final, abs(Recibo_Cte)), colour="YELLOW") +
  geom_point(aes(Fecha_Final, abs(Recibo_Otr)), colour="BLACK") +
  geom_point(aes(Fecha_Final, abs(Gasto_Cte)), colour="BLUE") +
  geom_point(aes(Fecha_Final, abs(Gasto_Otr)), colour="RED")

#  
#                                      # Barras Superpuestas desde 0, no stacked
#                                      # GRÁFICO BARRAS POR MESES CON INGRESOS, GASTOS 
#                                                            # con geom_bar   
#  ggplot(REFLAG) +                       
#  #  geom_bar(aes(Fecha_Final, abs(Transferencias)), stat="identity", fill="DARKSALMON") +
#    geom_line(aes(Fecha_Final, Transferencias+Gastos),  colour="BLACK") +
#    geom_bar(aes(Fecha_Final, abs(Gastos)), stat="identity", fill="DEEPSKYBLUE", alpha=0.2) +
#    geom_bar(aes(Fecha_Final, abs(Comunidad)), stat="identity", fill="DARKGREY")
#  
#                                                            # con geom_col
#  ggplot(REFLAG) +                        
#  #  geom_col(aes(Fecha_Final, Transferencias), fill="RED", alpha=0.2) +
#  #  geom_line(aes(Fecha_Final, Transferencias+Gastos), colour="BLACK",linetype = "dotdash") +
#    geom_col(aes(Fecha_Final, Gastos), fill="BLUE", alpha=0.2) +
#    geom_col(aes(Fecha_Final, Gasto_Corriente), fill="LIGHTGREEN", alpha=0.8) +
#    geom_col(aes(Fecha_Final, Comunidad), fill="YELLOW")
#  
ggplot(REFLAG, aes(Fecha_Final, -Gasto_Otr)) +
  geom_line()            

ggplot(REFLAG, aes(Fecha_Final, -Gasto_Otr)) +
  geom_point()

ggplot(REFLAG, aes(Fecha_Final, -Gasto_Otr)) +
  geom_point(shape=11, colour="RED", size=2)+
  geom_segment(aes(x = Fecha_Final, y = 0, xend = Fecha_Final, yend = -Gasto_Otr))

ggplot(REFLAG)+
  geom_col(aes(x=Fecha_Final, y=-Gasto_Otr))

ggplot(REFLAG)+
  geom_col(aes(Fecha_Final,-Gasto_Otr), fill="PINK")

# Barras y Líneas, Totales y Medias
ggplot(REFLAG,aes(x=Fecha_Final, y=abs(Gasto_Otr)))+
  geom_col()+
  geom_line(aes(y=abs(Recibo_Cte)), colour="YELLOW")+
  geom_line(aes(y=mean(abs(Recibo_Otr))), colour="ORANGE")+
  #  geom_line(aes(y=-G_Cte_MM3), colour="BLUE")+
  geom_col(aes(y=abs(Gasto_Cte), fill="PINK"), alpha=0.7)

#===============================================================================


XXX <- FLAG
XXX <- XXX %>% filter(!Nomina,!Patrimonio)
XXX <- XXX %>% group_by(Fecha_Final, Categoria) %>% 
               summarise(Suma=sum(Importe))

ggplot(XXX, aes(x=Fecha_Final, y=-Suma, fill=Categoria)) +
  geom_col()+
  geom_point(aes(x=Fecha_Final, y=Nomina),
             data=REFLAG,
             inherit.aes = FALSE)+  
  geom_segment(aes(x = Fecha_Final, y = 0, xend = Fecha_Final, yend = Nomina),
               data = REFLAG,
               inherit.aes = FALSE)


ggplot(XXX, aes(x=Fecha_Final, y=-Suma, fill=Categoria)) +
  geom_col(position="dodge")+
  geom_point(aes(x=Fecha_Final, y=Nomina),
             data=REFLAG,
             inherit.aes = FALSE)+  
  geom_segment(aes(x = Fecha_Final, y = 0, xend = Fecha_Final, yend = Nomina),
               data = REFLAG,
               inherit.aes = FALSE)


#===============================================================================

# GRAFICOS SOBRE FLAG CON BARRAS APILADAS POR CATEGORIA
# Sin filtrar antes


# Desde FLAG Compensación de Gasto sale como ingreso
# Desde FLAG no netea importes por Categoría,
# sino pinta dobles movimientos + y -
# de la misma Categoría (p.e. en Gasto_Corriente)
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
  geom_point(aes(x=Fecha_Final, y=Nomina), data=REFLAG, inherit.aes = FALSE)


ggplot()+
  geom_col(aes(x=Fecha_Final, y=-Suma, fill=Categoria), data=GROUPED_FLAG)+
  geom_point(aes(x=Fecha_Final, y=Nomina), data=REFLAG)+
  geom_line(aes(x=Fecha_Final, y=Nomina), data=REFLAG, colour="DARKGREY")+
  geom_segment(aes(x = Fecha_Final, y = 0, xend = Fecha_Final, yend = Nomina),
               data = REFLAG,
               inherit.aes = FALSE)#esta linea sobra porque no hereda nada
                                   #de la linea inicial de ggplot

ggplot()+
  geom_col(aes(x=Fecha_Final, y=-Suma, fill=Categoria), data=GROUPED_FLAG)+
  #  geom_point(aes(x=Fecha_Final, y=-G_Cte_MM3), data=REFLAG)+
  #  geom_line(aes(x=Fecha_Final, y=-G_Cte_MM3), data=REFLAG, colour="YELLOW")+
  geom_segment(aes(x = Fecha_Final, y = 0, xend = Fecha_Final, yend = -G_Cte_MM3),
               data = REFLAG,
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
