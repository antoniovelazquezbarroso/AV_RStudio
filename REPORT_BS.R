#source("LOAD_BS.R")
source("FLAG_BS.R")
# source("CHECK_BS.R") # ¿AÑADIRLE UN MENSAJE PARA CASO DE ERRORES?
# No existe CHECK_BS.R, habrá que crearlo si conviene

#===============================================================================

#    AÑADIR CATEGORIAS AGREGADAS ( O DESAGREGADAS ) PARA INFORMES Y GRÁFICOS

FLAG <- FLAG %>% mutate(
                        Recibos = Comunidad | Telefono | Luz ,
                        Gastos =  Gasto_Corriente | Gasto_Otro ,
                        Total_Gasto = Recibos | Servicio | Gastos ,
                        Total_Fijo = Recibos | Servicio | Gasto_Corriente
                       )  

#==========================================================================

# AÑADE AL MOVIMIENTO Fecha_Final DE PERIODO 
# para agrupar los datos en graficos o cuadros-resumen

# Aunque el periodo de reporting (por mes, trimestre, ...) es parte del REPORT
# la Fecha_Final se añade en FLAG para poderla utilizar en gráficos de barras
# acumulados por Categorías, que se hacen desde FLAG (movimientos individuales)

#  por meses
FLAG <- FLAG %>% mutate(Fecha_Final=as_date(ceiling_date(Fecha, unit = "month")-1))  

#  cambiando month por quarter para trimestres  
#FLAG <- FLAG %>% mutate(Fecha_Final=as_date(ceiling_date(Fecha, unit = "quarter")-1))  

#===============================================================================

#        TOTALIZANDO IMPORTES POR FECHA_FINAL Y CATEGORIA DESDE VARIABLES FILTRO

# FLAG <- FLAG %>% filter(Fecha_Final>"2022-09-30") # Filtra antes 
REPORT <- FLAG %>% 
  arrange(Fecha_Final) %>%               # Asegurar el orden por fechas
  group_by(Fecha_Final) %>%    # Calcular Totales por Fecha y Categoria 
  summarise(                   # (lo que elijas definir en el "summarise"
    #Numero=n(),        #  para gráficos o cuadros-resumen )       
    #Suma=sum(Importe), 
    #Cobros=sum(Importe[Importe>0]),
    #Pagos=sum(Importe[Importe<0]),
    Comunidad=sum(Importe[Comunidad]),
    Telefono=sum(Importe[Telefono]),
    Luz=sum(Importe[Luz]),
    Servicio=sum(Importe[Servicio]),
    Gasto_Corriente=sum(Importe[Gasto_Corriente]),
    # No sirve, promedia entre operaciones dentro del periodo
    # avGasto_Corriente=mean(Importe[Gasto_Corriente]), 
    # sdGasto_Corriente=sd(Importe[Gasto_Corriente]),
    Gasto_Otro=sum(Importe[Gasto_Otro]),
    Total_Gasto=sum(Importe[Total_Gasto]), 
    Transferencias=sum(Importe[Transferencias]),
    Check=sum(Importe[Check]),
    Recibos=sum(Importe[Recibos]),
    Gastos=sum(Importe[Gastos]),
    Total_Fijo=sum(Importe[Total_Fijo])
  ) # %>% 
               # Puedes elegir los campos de salida, su orden, sus nombres ...
#  select(Fecha_Final,
#          Recibos,
#          Servicio,
#          Gastos,
#          Total_Gasto,
#          Transferencias
#         )
#
# select(Fecha_Final:Total_Gasto)

#===============================================================================                                                              
#                       TRASPONER ESTE DATA FRAME
#   
#   TO_EXCEL <- pivot_longer(REPORT,cols = (-Fecha_Final), names_to="Categoria") %>% 
#     pivot_wider(names_from=c(Fecha_Final))
#
#   # PARA EXPORTAR A EXCEL
#   library(writexl)
#   write_xlsx(TO_EXCEL, 'data/TO_EXCEL.xlsx')
#   
#===============================================================================
#
#          TOTALIZANDO IMPORTES POR CATEGORIA DESDE VARIABLE CATEGORÍA
#          (HACE LO MISMO QUE PARA CONSTRUIR REPORT, Y EL RESULTADO ES IGUAL)
#         #  Podría servir si se quiere eliminar las variables Filtro del Dataset
#  
#  REPORT2 <- REPORT %>%  
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
#===============================================================================
