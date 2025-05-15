#source("LOAD_ING.R")
source("FLAG_ING.R")
# source("CHECK_ING.R") # ¿AÑADIRLE UN MENSAJE PARA CASO DE ERRORES?

#===============================================================================

#    AÑADE AL MOVIMIENTO CATEGORIAS AGREGADAS ( O DESAGREGADAS ) 
#    ESPECÍFICAS PARA INFORMES Y GRÁFICOS

FLAG <- FLAG %>% mutate(
          Total_Gasto = Casa | Recibo_Cte | Gasto_Cte | Recibo_Otr | Gasto_Otr,
          Total_sin_Casa = Recibo_Cte |Gasto_Cte | Recibo_Otr | Gasto_Otr,  
          Recibos = Recibo_Cte | Recibo_Otr,
          Gastos = Gasto_Cte | Gasto_Otr,
          Total_Fijo = Recibo_Cte | Gasto_Cte | Recibo_Otr 
                        )

#    Comunidad = (Categoria == "Recibos")&
#                (grepl("CARGO DE RECIBOS", Descripcion)&
#                 (grepl("Recibo Cp Rfv", Concepto)|
#                  grepl("Recibo Raimundo Fernandez Villaverde", Concepto)|
#                  grepl("Recibo Geminis I, Garaje", Concepto)
#                  )
#                 )
#    Telefono = (Categoria == "Recibos")&
#               (grepl("CARGO DE RECIBOS", Descripcion)&
#                grepl("Recibo Yoigo", Concepto)
#               )
#===============================================================================

#     AÑADE AL MOVIMIENTO Fecha_Final DE PERIODO 
#     Para agrupar los datos en graficos o cuadros-resumen.

# Aunque el periodo de reporting (por mes, trimestre, ...) es parte del REPORT
# la Fecha_Final se añade en FLAG para poderla utilizar en gráficos de barras
# acumulados por Categorías, que se hacen desde FLAG (movimientos individuales)

#  por meses
FLAG <- FLAG %>%
  mutate(Fecha_Final=as_date(ceiling_date(Fecha, unit = "month")-1))  

#  cambiando month por quarter para trimestres  
#FLAG <- FLAG %>%
#        mutate(Fecha_Final=as_date(ceiling_date(Fecha, unit = "quarter")-1))  

#==============================================================================

#     TOTALIZANDO IMPORTES POR FECHA_FINAL 
#     Y POR CATEGORIA DESDE VARIABLES FILTRO,
#     O CALCULANDO NUEVOS VALORES (p.e. TOTALES O SUBTOTALES DESDE MOVIMIENTOS)

REPORT <- FLAG %>% 
# filter(Fecha_Final > "2022-09-30",
#        Fecha_Final =< "2025-03-31") %>%  # Elegir Fechas de Inicio y/o Final
# filter(Total_Gasto)         # Cualquier otro Filtro (Gastos, Recibos, Grandes)                    
  arrange(Fecha_Final) %>%                 # Asegurar el orden por fechas
  group_by(Fecha_Final) %>%           # Calcular Totales por Fecha y Categoria 
  summarise(                          #(lo que elijas definir en el "summarise"
            #Numero=n(),                
            #Suma=sum(Importe),        #  para gráficos o cuadros-resumen )
            #Cobros=sum(Importe[Importe>0]),
            #Pagos=sum(Importe[Importe<0]),
            #Nomina=sum(Importe[Nomina]),
            Recibo_Cte=sum(Importe[Recibo_Cte]),    
            Recibo_Otr=sum(Importe[Recibo_Otr]),
            Gasto_Cte=sum(Importe[Gasto_Cte]),
            Gasto_Otr=sum(Importe[Gasto_Otr]),            
            Casa=sum(Importe[Casa]),
            Total_Gasto=sum(Importe[Total_Gasto]),            
            #Otro_Total_Gasto=Casa+Recibo_Cte+Gasto_Cte+Recibo_Otr+Gasto_Otr,            
            Total_sin_Casa=sum(Importe[Total_sin_Casa]),
            #Otro_Total_sin_Casa=Recibo_Cte+Gasto_Cte+Recibo_Otr+Gasto_Otr,
            Recibos=sum(Importe[Recibos]),            
            Gastos=sum(Importe[Gastos]),            
            Total_Fijo=sum(Importe[Total_Fijo]),
            #Patrimonio=sum(Importe[Patrimonio]),                 
            # Promedia y analiza variacion entre movimientos dentro del periodo
            #avGasto_Cte=mean(Importe[Gasto_Cte]), 
            #sdGasto_Cte=sd(Importe[Gasto_Cte]),
            Check=sum(Importe[Check]),
            #OtroCheck= near( Suma,
            #                 (Nomina + 
            #                    Casa +
            #                    Recibo_Cte +
            #                    Gasto_Cte +
            #                    Recibo_Otr +
            #                    Gasto_Otr +
            #                    Patrimonio
            #                 )
            #                )
  ) 

#  ) #  %>% 
               # Puedes elegir los campos de salida, su orden, sus nombres ...

#  select(Fecha_Final, Casa, Recibo_Cte, Recibo_Otr, ... , Patrimonio)
#  select(Fecha_Final, Recibo_Cte:Total_Fijo)

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
#===============================================================================                                                              

