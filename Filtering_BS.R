library(tidyverse)

source("LOAD_BS.R")

LAB <- BS
#===================================================================================================
                        # VER MOVIMIENTOS POR CODIGO-DESCRIPCION

LAB <-BS %>% group_by(Descripcion, Codigo) %>%
      summarise(Num=n(),Suma=sum(Importe)) %>%
      arrange(desc(Num)) %>%
      select(Num,Descripcion, Codigo, Suma) %>%
      print(n=nrow(BS))

LAB <- BS %>% filter(grepl("CARGO DE OPERACION CON TARJETA", Descripcion))

LAB <- BS %>% filter(grepl("TRANSFERENCIAS RECIBIDAS", Descripcion))

LAB <- BS %>% filter(grepl("CARGO DE RECIBOS", Descripcion))

LAB <- BS %>% filter(grepl("TRANSFERENCIAS EMITIDAS", Descripcion))

LAB <-  BS %>% filter(                    # OPERACIONES CODIGOS RAROS
                      grepl("ABONO DE OPERACION CON TARJETA", Descripcion)|
                      grepl("ABONOS VARIOS CONCEPTOS", Descripcion)|
                      grepl("ADEUDO INTER/COMIS/GASTOS", Descripcion)|
                      grepl("PAGO EN EFECTIVO", Descripcion)
                     )

            # No debe haber  Movimientos SIN CLASIFICAR POR CODIGO-DESCRIPCION
LAB <- BS %>% filter(
                     !grepl("CARGO DE OPERACION CON TARJETA", Descripcion)&
                     !grepl("TRANSFERENCIAS RECIBIDAS", Descripcion)&
                     !grepl("CARGO DE RECIBOS", Descripcion)&
                     !grepl("TRANSFERENCIAS EMITIDAS", Descripcion)&
                     !grepl("ABONO DE OPERACION CON TARJETA", Descripcion)&
                     !grepl("ABONOS VARIOS CONCEPTOS", Descripcion)&
                     !grepl("ADEUDO INTER/COMIS/GASTOS", Descripcion)&
                     !grepl("PAGO EN EFECTIVO", Descripcion)
                    )

#===================================================================================================
                                           # Definir FILTROS por Categoría de Movimientos


  
#            # Transferencias Recibidas todas
#  LAB <- BS %>% filter(grepl("TRANSFERENCIAS RECIBIDAS", Descripcion))

            # Transferencias a casa (excluyendo menores de € 200)
            # las menores van a Tarjeta_Cte (por compensación gastos)

                                                                # Transferencias
LAB <- BS %>% filter(grepl("TRANSFERENCIAS RECIBIDAS", Descripcion)&
                     !abs(Importe) <= 200
                    )


# #            # Transferencias Emitidas todas                   
# LAB <- BS %>% filter(grepl("TRANSFERENCIAS EMITIDAS", Descripcion))
# 
#
# #            # Transferencias Emitidas hasta €200   van a Gasto_Corriente
# LAB <- BS %>% filter(grepl("TRANSFERENCIAS EMITIDAS", Descripcion)&
#                            abs(Importe) <= 200                        
#                     )
# 
# #            # Transferencias Emitidas no menoresc a 200  van a Gasto_Otro
# LAB <- BS %>% filter(grepl("TRANSFERENCIAS EMITIDAS", Descripcion)&
#                      !abs(Importe) < 200                    
# )


              # Reintegros Cajero hasta €500                          # Servicio
LAB <- BS %>% filter(
                     grepl("CARGO DE OPERACION CON TARJETA", Descripcion)&
                     grepl("Reint", Concepto)&
                     !abs(Importe) > 500
                    )

#           # Recibos todos 
# LAB <- BS %>% filter(grepl("CARGO DE RECIBOS", Descripcion))

                                               # Recibos Comunidad Casa y Garaje
LAB <- BS %>% filter(grepl("CARGO DE RECIBOS", Descripcion)&
                     (grepl("Recibo Cp Rfv", Concepto)|
                      grepl("Recibo Raimundo Fernandez Villaverde", Concepto)|
                      grepl("Recibo Geminis I, Garaje", Concepto)  
                      )
                     )
                                                              # Recibos Telefono
LAB <- BS %>% filter(grepl("CARGO DE RECIBOS", Descripcion)&
                     grepl("Recibo Yoigo", Concepto)
                     )
                                                           # Resto Recibos (LUZ) 
LAB <- BS %>% filter(grepl("CARGO DE RECIBOS", Descripcion)&
                       (!grepl("Recibo Cp Rfv", Concepto)&
                        !grepl("Recibo Raimundo Fernandez Villaverde", Concepto)&
                        !grepl("Recibo Geminis I, Garaje", Concepto)&
                        !grepl("Recibo Yoigo", Concepto)
                       )
                     )                             

#             # Cargos Tarjeta Todos
# LAB <- BS %>% filter(grepl("CARGO DE OPERACION CON TARJETA", Descripcion))

#             # Reintegros Cajero todos
# LAB <- BS %>% filter(
#                      grepl("CARGO DE OPERACION CON TARJETA", Descripcion)&
#                      grepl("Reint", Concepto) 
#                     )


           # Cargos Tarjeta excepto Cajeros y hasta €200      
           # Transferecias emitidas hasta €200
           # Transferencias recibidas hasta €200 (compensacion gastos)
           # Conceptos Atípicos (normalmente devoluciones,compensacion gastos) 

                                                               # Gasto_Corriente
LAB <- BS %>% filter(
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
                    ) 

             # Cargos Tarjeta excepto Cajeros y mayores de €200       
             # Transferencias Emitidas mayores de €200 (menores van a Tarjeta_Cte)
             # Retiradas Cajero mayores de €500 (los menores van a Servicio)

                                                                     # Gasto_Otro 
LAB <- BS %>% filter(
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
                    )  

                                     # ¿ABRIR TARJETA POR CONCEPTO O POR IMPORTE?

#                                                     # ABIERTO POR 4 CONCEPTOS
# 
#                                               # Reintegros Cajero (Pilar y otros)
# LAB <- BS %>% filter(
#                       grepl("CARGO DE OPERACION CON TARJETA", Descripcion),
#                       grepl("Reint", Concepto)
#                     )
# 
# LAB <- BS %>% filter(
#                      grepl("CARGO DE OPERACION CON TARJETA", Descripcion),
#                      grepl("Transaccion Contactless En", Concepto)
#                     )
# 
# LAB <- BS %>% filter(
#                      grepl("CARGO DE OPERACION CON TARJETA", Descripcion),
#                      grepl("Compra", Concepto)
#                     )
# 
# LAB <- BS %>% filter(
#                      grepl("CARGO DE OPERACION CON TARJETA", Descripcion),
#                      grepl("Pago Movil En", Concepto)
#                     )                   
# 
#
#
#                          Son devoluciones, van dentro de Tarjeta_Cte
# LAB <-  BS %>% filter(                                   # OPERACIONES CODIGOS RAROS
#                       grepl("ABONO DE OPERACION CON TARJETA", Descripcion)|
#                       grepl("ABONO VARIOS CONCEPTOS", Descripcion)|
#                       grepl("ADEUDO INTER/COMIS/GASTOS", Descripcion)|
#                       grepl("PAGO EN EFECTIVO", Descripcion)
#                      )


                       
                       