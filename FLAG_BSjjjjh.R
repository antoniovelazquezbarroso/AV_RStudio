library(writexl)
source("LOAD_BS.R")
FLAG <- BS
#===============================================================================
                               #  Eliminar columnas innecesarias por simplificar 
FLAG <- FLAG %>% select(- NumOrden, -Saldo, -Codigo)

#===============================================================================
                         # Definir variables FILTRO por Categoría de Movimientos
FLAG <- FLAG %>%
       #Inicio de mutate
       mutate(
              # Transferencias a casa (excluyendo menores de € 200)
              # las menores van a Tarjeta_Cte (por compensación gastos)
                                                                # Transferencias
              Transferencias=(grepl("TRANSFERENCIAS RECIBIDAS", Descripcion)&
                              !abs(Importe) <= 200
                             ),                 

              # Reintegros Cajero hasta €500 para pagar Pilar
              # Los mayores a €500 van a Gasto_Otro
                                                                      # Servicio
              Servicio=(
                        grepl("CARGO DE OPERACION CON TARJETA", Descripcion)&
                        grepl("Reint", Concepto)&
                        !abs(Importe) > 500
                       ),

              # Recibos                            
                                                               # Recibos (todos)
              Recibos=(grepl("CARGO DE RECIBOS", Descripcion)),         
            
              # Cargos Tarjeta excepto Cajeros y hasta €200      
              # Transferecias emitidas hasta €200
              # Transferencias recibidas hasta €200 (compensacion gastos)
              # Conceptos Atípicos (devoluciones,compensacion gastos)
                                                               # Gasto_Corriente             
              Gasto_Corriente=(
                               (grepl("CARGO DE OPERACION CON TARJETA", Descripcion)&
                                !grepl("Reint", Concepto)&
                                abs(Importe) <= 200
                               )|
                               (grepl("TRANSFERENCIAS EMITIDAS", Descripcion)&
                                abs(Importe) <= 200
                               )|
                               (grepl("TRANSFERENCIAS RECIBIDAS", Descripcion)&
                                abs(Importe) <= 200
                               )|                               
                               (grepl("ABONO DE OPERACION CON TARJETA", Descripcion)|
                                grepl("ABONOS VARIOS CONCEPTOS", Descripcion)|
                                grepl("ADEUDO INTER/COMIS/GASTOS", Descripcion)|
                                grepl("PAGO EN EFECTIVO", Descripcion)                       
                               ) 
                              ), 
              
              # Cargos Tarjeta excepto Cajeros y mayores de €200       
              # Transferencias Emitidas mayores de €200 (menores van a Tarjeta_Cte)
              # Retiradas Cajero mayores de €500 (los menores van a Servicio)
                                                                    # Gasto_Otro             
              Gasto_Otro=(
                          (grepl("CARGO DE OPERACION CON TARJETA", Descripcion)&
                           !grepl("Reint", Concepto)&
                           !abs(Importe) <= 200
                          )|
                          (grepl("TRANSFERENCIAS EMITIDAS", Descripcion)&
                           !abs(Importe) <= 200
                          )|                            
                          (grepl("CARGO DE OPERACION CON TARJETA", Descripcion)&
                           grepl("Reint", Concepto)&
                           abs(Importe) > 500
                          )
                         ),              
              
              #    COMPROBACIÓN NO HAY MOVIMIENTOS SIN CLASIFICAR       # Check
              Check=(
                     !Transferencias &
                     !Recibos &
                     !Servicio &
                     !Gasto_Corriente &
                     !Gasto_Otro 
                    )            
             ) # Fin de mutate  

#===============================================================================
#  
#          CHECK no hay movimientos Check, sin variable de Categoría asignada
#  CHECK <- FLAG %>% summarise(num=n(),tot=sum(Importe[Check]))
#  CHECK$tot == 0
#  remove(CHECK)
#   
#  #       CHECK NO HAY MOVIMIENTOS CON MÁS DE UNA CATEGORIA ASIGNADA
#  #       La suma de importes de todos los movimientos es igual a
#  #       la suma de los totales de todas las categorías
#  (
#  CHECK <- near(sum(FLAG$Importe), sum(FLAG$Importe[FLAG$Transferencias==TRUE])+
#                                    sum(FLAG$Importe[FLAG$Servicio==TRUE])+
#                                    sum(FLAG$Importe[FLAG$Recibos==TRUE])+
#                                    sum(FLAG$Importe[FLAG$Gasto_Corriente==TRUE])+
#                                    sum(FLAG$Importe[FLAG$Gasto_Otro==TRUE])+
#                                    sum(FLAG$Importe[FLAG$Check==TRUE]))
#  )
#  remove(CHECK)

#  (
#  CHECK <- FLAG$Transferencias +
#           FLAG$Servicio +
#           FLAG$Recibos +
#           FLAG$Gasto_Corriente +
#           FLAG$Gasto_Otro == 1
#  )
#  CHECK[FALSE]  # No debe haber ningún FALSE
#  remove(CHECK)
#===============================================================================

                    # Definir variable Categoría segun el FILTRO que aplica
FLAG <- FLAG %>%
#Inicio de mutate
mutate(Categoria = case_when( #Inicio de case_when

# Transferencias   
(grepl("TRANSFERENCIAS RECIBIDAS", Descripcion)&
   !abs(Importe) <= 200
)                                                           ~ "Transferencias",        

# Servicio
(grepl("CARGO DE OPERACION CON TARJETA", Descripcion)&
 grepl("Reint", Concepto)&
 !abs(Importe) > 500
)                                                           ~ "Servicio",

# Recibos
(grepl("CARGO DE RECIBOS", Descripcion))                    ~ "Recibos",

# Gasto_Corriente             
(
 (grepl("CARGO DE OPERACION CON TARJETA", Descripcion)&
    !grepl("Reint", Concepto)&
    abs(Importe) <= 200
 )|
 (grepl("TRANSFERENCIAS EMITIDAS", Descripcion)&
    abs(Importe) <= 200
 )|
 (grepl("TRANSFERENCIAS RECIBIDAS", Descripcion)&
    abs(Importe) <= 200
 )|                               
 (grepl("ABONO DE OPERACION CON TARJETA", Descripcion)|
    grepl("ABONOS VARIOS CONCEPTOS", Descripcion)|
    grepl("ADEUDO INTER/COMIS/GASTOS", Descripcion)|
    grepl("PAGO EN EFECTIVO", Descripcion)                       
 ) 
)                                                          ~ "Gasto_Corriente",  

# Gasto_Otro             
(
 (grepl("CARGO DE OPERACION CON TARJETA", Descripcion)&
    !grepl("Reint", Concepto)&
    !abs(Importe) <= 200
 )|
 (grepl("TRANSFERENCIAS EMITIDAS", Descripcion)&
    !abs(Importe) <= 200
 )|                            
 (grepl("CARGO DE OPERACION CON TARJETA", Descripcion)&
    grepl("Reint", Concepto)&
    abs(Importe) > 500
 )
)                                                             ~ "Gasto_Otro", 

(TRUE)                                                        ~ "NC"                           
                            )# Fin de case_when
      )# Fin de mutate 

#================================================================================

#   #       CHECK no hay movimientos NC, sin variable de Categoría asignada
#   CHECK <- FLAG %>%
#            filter(Categoria == "NC") %>% 
#            summarise(num=n(),tot=sum(Importe))
#   CHECK$tot == 0
#   remove(CHECK)

     # Todas las Categorías, por su orden default
#   Cat=sort(unique(FLAG$Categoria))
#   Cat
#   # "Gasto_Corriente" "Gasto_Otro"  "Recibos" "Servicio" "Transferencias"
#   remove(Cat)

 
 #       CHECK NO HAY MOVIMIENTOS CON MÁS DE UNA CATEGORIA ASIGNADA
 #       La suma de importes de todos los movimientos es igual a
 #       la suma de los totales de todas las categorías
 #  (
 #  CHECK <- near(sum(FLAG$Importe), 
 #                (sum(FLAG$Importe[FLAG$Categoria == "Transferencias"])+
 #                 sum(FLAG$Importe[FLAG$Categoria == "Servicio"])+
 #                 sum(FLAG$Importe[FLAG$Categoria == "Recibos"])+
 #                 sum(FLAG$Importe[FLAG$Categoria == "Gasto_Corriente"])+
 #                 sum(FLAG$Importe[FLAG$Categoria == "Gasto_Otro"])
 #                )
 #               )
 #  )
 #  remove(CHECK)

#          La suma de todas las Categorías (binarias) de un Mov es 1
#  (
#    CHECK <- FLAG$Transferencias +
#      FLAG$Servicio +
#      FLAG$Recibos +
#      FLAG$Gasto_Cte +
#      FLAG$Gasto_Otr +
#      FLAG$Patrimonio == 1
#  )
#  CHECK[FALSE]
#  remove(CHECK)
 
# ========  HASTA AQUÍ PARA CLASIFICAR Y ETIQUETAR POR CATEGORÍAS ==============
 
#===============================================================================

#    AÑADIR CATEGORIAS AGREGADAS ( O DESAGREGADAS ) PARA INFORMES Y GRÁFICOS

FLAG <- FLAG %>% mutate(
                        Gastos = Servicio | Recibos | Gasto_Corriente | Gasto_Otro,
                        
                        Comunidad = (Categoria == "Recibos")&
                                     (grepl("CARGO DE RECIBOS", Descripcion)&
                                       (grepl("Recibo Cp Rfv", Concepto)|
                                        grepl("Recibo Raimundo Fernandez Villaverde", Concepto)|
                                        grepl("Recibo Geminis I, Garaje", Concepto)
                                       )
                                      ),
                        Telefono=(Categoria == "Recibos")&
                                  (grepl("CARGO DE RECIBOS", Descripcion)&
                                    grepl("Recibo Yoigo", Concepto)
                                  ),
                        Luz=(grepl("CARGO DE RECIBOS", Descripcion)&  # Resto Recibos (LUZ)
                             (!grepl("Recibo Cp Rfv", Concepto)&
                                !grepl("Recibo Raimundo Fernandez Villaverde", Concepto)&
                                !grepl("Recibo Geminis I, Garaje", Concepto)&
                                !grepl("Recibo Yoigo", Concepto)
                             )
                            )
                        )  
                        
#==========================================================================

            # AÑADE AL MOVIMIENTO Fecha_Final DE PERIODO 
            # para agrupar los datos en graficos o cuadros-resumen

  #  por meses
#FLAG <- FLAG %>% mutate(Fecha_Final=as_date(ceiling_date(Fecha, unit = "month")-1))  
  
  #  cambiando month por quarter para trimestres  
FLAG <- FLAG %>% mutate(Fecha_Final=as_date(ceiling_date(Fecha, unit = "quarter")-1))  

#================================================================================

#           TOTALIZANDO IMPORTES POR FECHA_FINAL Y CATEGORIA DESDE VARIABLES FILTRO
FLAG <- FLAG %>% filter(Fecha_Final>"2022-12-31")
REFLAG <- FLAG %>% 
         arrange(Fecha_Final) %>%                 # Asegurar el orden por fechas
         group_by(Fecha_Final) %>%      # Calcular Totales por Fecha y Categoria 
         summarise(Numero=n(),          # (lo que elijas definir en el "summarise" 
                   Suma=sum(Importe),   #  para gráficos o cuadros-resumen )
                   Cobros=sum(Importe[Importe>0]),
                   Pagos=sum(Importe[Importe<0]),
                   Transferencias=sum(Importe[Transferencias]),
                   Servicio=sum(Importe[Servicio]),
                   Recibos=sum(Importe[Recibos]),
                   Gasto_Corriente=sum(Importe[Gasto_Corriente]),
                     # No sirve, promedia por numero de operaciones dentro periodo
                   #avGasto_Corriente=mean(Importe[Gasto_Corriente]), 
                   #sdGasto_Corriente=sd(Importe[Gasto_Corriente]),
                   Gasto_Otro=sum(Importe[Gasto_Otro]),
                   Check=sum(Importe[Check]),
                   Gastos=sum(Importe[Gastos])
                  ) # %>% 
#         select(Fecha_Final, Transferencias, Gasto_Otro, Gasto_Corriente, Recibos, Servicio)
#===============================================================================

#            CALCULANDO VALORES A PARTIR DE LOS TOTALES, AÑADIRLOS A REFLAG

REFLAG$Gasto_Corriente
sum(REFLAG$Gasto_Corriente)
( Anterior <- lag(REFLAG$Gasto_Corriente) )
mean(REFLAG$Gasto_Corriente) # Promedia los totales de todos los periodos
sd(REFLAG$Gasto_Corriente)
mean(REFLAG$Gasto_Otro)
sd(REFLAG$Gasto_Otro)

MM3 <- function(MiVar){
                       (MiVar+lag(MiVar, n=1)+lag(MiVar, n=2))/3
                      }
MM3(REFLAG$Gasto_Corriente) # Promedia los totales de los 3 últimos periodos

                 # Añadiendo campos calculados para un cálculo ad-hoc
REFLAG %>% mutate(G_Cte_lag=lag(Gasto_Corriente),
                   MEAN=mean(REFLAG$Gasto_Corriente),
                   MiMM3=MM3(Gasto_Corriente)
                  ) %>% 
           select(Fecha_Final, Gasto_Corriente, G_Cte_lag, MEAN, MiMM3) %>% 
           print(n=nrow(REFLAG))

                  # # Añadiendo campos calculados al propio REFLAG
REFLAG <- REFLAG %>% mutate(G_Cte_MM3=MM3(Gasto_Corriente))

#=================================================================================

#          TOTALIZANDO IMPORTES POR CATEGORIA DESDE VARIABLE CATEGORÍA
#          (HACE LO MISMO QUE PARA CONSTRUIR REFLAG, Y EL RESULTADO ES IGUAL)


#         #  Podría servir si se quiere eliminar las variables Filtro del Dataset
#  
#  REFLAG2 <- FLAG %>%          
#    #          # ¿ Quizá filtrando antes, para gráficos y cuadros-resumen ad-hoc
#    #          #  Hace falta también adaptar el "summarise"
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

# TRASPONER ESTE DATA FRAME

BALER <- pivot_longer(REFLAG,cols = (-Fecha_Final), names_to="Categoria") %>% 
         pivot_wider(names_from=c(Fecha_Final))

# PARA EXPORTAR A EXCEL
write_xlsx(BALER, 'data/BALER.xlsx')

#=================================================================================                                                              
                                                 #    GRAFICOS SOBRE REFLAG


ggplot(REFLAG) +                      # GRÁFICO LINEAS POR MESES CON INGRESOS, GASTOS 
#  geom_line(aes(Fecha_Final, abs(Transferencias)), colour="RED", linetype = "dotted") +
  geom_line(aes(Fecha_Final, abs(Gastos)), colour="BLUE",linetype = "dotted") +
  geom_line(aes(Fecha_Final, abs(Gastos)-abs(Gasto_Otro)), colour="DARKGREY",linetype = "dotdash") +
  geom_line(aes(Fecha_Final, abs(Servicio)), colour="GREEN") +
  geom_line(aes(Fecha_Final, abs(Recibos)), colour="BLACK",linetype = "dotdash") +
  geom_line(aes(Fecha_Final, abs(Gasto_Corriente)), colour="BLUE") +
  geom_line(aes(Fecha_Final, abs(Gasto_Otro)), colour="RED")


ggplot(REFLAG) +                      # GRÁFICO PUNTOS POR MESES CON INGRESOS, GASTOS 
  #  geom_point(aes(Fecha_Final, abs(Transferencias)), colour="RED") +
  geom_point(aes(Fecha_Final, abs(Gastos)), colour="BLUE",linetype = "dotted") +
  #  geom_point(aes(Fecha_Final, abs(Gastos)-abs(Gasto_Otro)), colour="RED") +
  geom_point(aes(Fecha_Final, abs(Servicio)), colour="GREEN") +
  geom_point(aes(Fecha_Final, abs(Recibos)), colour="BLACK") +
  geom_point(aes(Fecha_Final, abs(Gasto_Corriente)), colour="BLUE") +
  geom_point(aes(Fecha_Final, abs(Gasto_Otro)), colour="RED")

#  
#                                      # Barras Superpuestas desde 0, no stacked
#                                      # GRÁFICO BARRAS POR MESES CON INGRESOS, GASTOS 
#                                                            # con geom_bar   
#  ggplot(REFLAG) +                       
#  #  geom_bar(aes(Fecha_Final, abs(Transferencias)), stat="identity", fill="DARKSALMON") +
#    geom_line(aes(Fecha_Final, Transferencias+Gastos),  colour="BLACK") +
#    geom_bar(aes(Fecha_Final, abs(Gastos)), stat="identity", fill="DEEPSKYBLUE", alpha=0.2) +
#    geom_bar(aes(Fecha_Final, abs(Recibos)), stat="identity", fill="DARKGREY")
#  
#                                                            # con geom_col
#  ggplot(REFLAG) +                        
#  #  geom_col(aes(Fecha_Final, Transferencias), fill="RED", alpha=0.2) +
#  #  geom_line(aes(Fecha_Final, Transferencias+Gastos), colour="BLACK",linetype = "dotdash") +
#    geom_col(aes(Fecha_Final, Gastos), fill="BLUE", alpha=0.2) +
#    geom_col(aes(Fecha_Final, Gasto_Corriente), fill="LIGHTGREEN", alpha=0.8) +
#    geom_col(aes(Fecha_Final, Recibos), fill="YELLOW")
#  
ggplot(REFLAG, aes(Fecha_Final, -Recibos)) +
  geom_line()            

ggplot(REFLAG, aes(Fecha_Final, -Recibos)) +
  geom_point()

ggplot(REFLAG, aes(Fecha_Final, Transferencias)) +
  geom_point(shape=11, colour="RED", size=2)+
  geom_segment(aes(x = Fecha_Final, y = 0, xend = Fecha_Final, yend = Transferencias))

#  ggplot(REFLAG)+
#    geom_bar(aes(x=Fecha_Final, y=-Recibos), stat="identity")

ggplot(REFLAG)+
  geom_col(aes(x=Fecha_Final, y=-Recibos))

ggplot(REFLAG)+
  geom_col(aes(Fecha_Final,-Recibos), fill="PINK")

                                             # Barras y Líneas, Totales y Medias
ggplot(REFLAG,aes(x=Fecha_Final, y=abs(Recibos)))+
  geom_col()+
  geom_line(aes(y=abs(Servicio)), colour="YELLOW")+
  geom_line(aes(y=mean(abs(Recibos))), colour="ORANGE")+
#  geom_line(aes(y=-G_Cte_MM3), colour="BLUE")+
  geom_col(aes(y=abs(Gasto_Otro), fill="PINK"), alpha=0.7)

#================================================================================

# ESTO SIRVE

FLAG <- FLAG %>% filter(Gastos)
XXX <- FLAG %>% group_by(Fecha_Final, Categoria) %>% 
                 summarise(Suma=sum(Importe))

ggplot(XXX, aes(x=Fecha_Final, y=-Suma, fill=Categoria)) +
  geom_col()+
  geom_point(aes(x=Fecha_Final, y=Transferencias),
             data=REFLAG,
             inherit.aes = FALSE)+  
  geom_segment(aes(x = Fecha_Final, y = 0, xend = Fecha_Final, yend = Transferencias),
               data = REFLAG,
               inherit.aes = FALSE)


ggplot(XXX, aes(x=Fecha_Final, y=-Suma, fill=Categoria)) +
  geom_col(position="dodge")+
  geom_point(aes(x=Fecha_Final, y=Transferencias),
             data=REFLAG,
             inherit.aes = FALSE)+  
  geom_segment(aes(x = Fecha_Final, y = 0, xend = Fecha_Final, yend = Transferencias),
               data = REFLAG,
               inherit.aes = FALSE)


#================================================================================

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
GROUPED_FLAG <- FLAG %>% filter(Gasto_Corriente) %>%
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
  geom_point(aes(x=Fecha_Final, y=Transferencias), data=REFLAG, inherit.aes = FALSE)
  

ggplot()+
  geom_col(aes(x=Fecha_Final, y=-Suma, fill=Categoria), data=GROUPED_FLAG)+
  geom_point(aes(x=Fecha_Final, y=Transferencias), data=REFLAG)+
  geom_line(aes(x=Fecha_Final, y=Transferencias), data=REFLAG, colour="DARKGREY")+
  geom_segment(aes(x = Fecha_Final, y = 0, xend = Fecha_Final, yend = Transferencias),
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
DDD <- FLAG %>% filter(Categoria=="Recibos")
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




