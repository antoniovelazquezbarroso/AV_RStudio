source("LOAD_BS_Ammended.R")

FLAG <- BS
#===============================================================================
                               #  Eliminar columnas innecesarias por simplificar 
FLAG <- FLAG %>% select(-NumOrden, -Saldo, -Codigo)

#===============================================================================
                         # Definir variables FILTRO por Categoría de Movimientos
FLAG <- FLAG %>%
  #Inicio de mutate
  mutate(
         # Recibos Comunidad Casa y Garaje
         Comunidad=(grepl("CARGO DE RECIBOS", Descripcion)&
                    (grepl("Recibo Cp Rfv", Concepto)|
                     grepl("Recibo Raimundo Fernandez Villaverde", Concepto)|
                     grepl("Recibo Geminis I, Garaje", Concepto)  
                    )
                   ),
         
         # Recibos Telefono
         Telefono=(grepl("CARGO DE RECIBOS", Descripcion)&
                   grepl("Recibo Yoigo", Concepto)
                  ),
         
         # Resto Recibos (LUZ) 
         Luz=(grepl("CARGO DE RECIBOS", Descripcion)&
              (!grepl("Recibo Cp Rfv", Concepto)&
               !grepl("Recibo Raimundo Fernandez Villaverde", Concepto)&
               !grepl("Recibo Geminis I, Garaje", Concepto)&
               !grepl("Recibo Yoigo", Concepto)
              )
             ),            

         # Servicio 
                    # Reintegros Cajero hasta €500 para pagar Pilar
                    # Los mayores a €500 van a Gasto_Otro
         Servicio=(grepl("CARGO DE OPERACION CON TARJETA", Descripcion)&
                   grepl("Reint", Concepto)&
                  !abs(Importe) > 500
                 )|
                 (grepl("PAGO EN EFECTIVO", Descripcion)&
                  grepl("Retirada De Efectivo En Cajero", Concepto)&
                  abs(Importe) < 500 &
                  abs(Importe) >= 370  
                 ),         
         
         # Gasto_Corriente 
                    # Cargos Tarjeta excepto Cajeros y hasta €200      
                    # Transferecias emitidas hasta €200
                    # Transferencias recibidas hasta €200 (compensacion gastos)
                    # Conceptos Atípicos (devoluciones,compensacion gastos)
            
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
            (grepl("PAGO EN EFECTIVO", Descripcion)&
     #        grepl("Retirada De Efectivo En Cajero", Concepto)&
             abs(Importe) < 370
            )
           )
                          ),
         
         # Gasto_Otro         
           # Cargos Tarjeta excepto Cajeros y mayores de €200       
           # Transferencias Emitidas mayores de €200 (menores van a Tarjeta_Cte)
           # Retiradas Cajero mayores de €500 (los menores van a Servicio)
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
            )|
           (grepl("PAGO EN EFECTIVO", Descripcion)&
     #          grepl("Retirada De Efectivo En Cajero", Concepto)&
               abs(Importe) >= 500
           )
 
                   ),
         # Transferencias
                      # Transferencias a casa (excluyendo menores de € 200)
                      # las menores van a Tarjeta_Cte (por compensación gastos)
                                                          
         Transferencias=(grepl("TRANSFERENCIAS RECIBIDAS", Descripcion)&
                         !abs(Importe) <= 200
                        ),                 

         #  COMPROBACIÓN NO HAY MOVIMIENTOS SIN CLASIFICAR       # Check
         Check=(
                !Comunidad &
                !Telefono &
                !Luz &
                !Servicio &
                !Gasto_Corriente &
                !Gasto_Otro &
                !Transferencias
               ) 
         )
        # Fin de mutate  
#===============================================================================
#  
#             CHECK no hay movimientos Check, sin variable de Categoría asignada
#  CHECK <- FLAG %>% summarise(num=n(),tot=sum(Importe[Check]))
#  CHECK$tot == 0
#  remove(CHECK)
#   
#             CHECK NO HAY MOVIMIENTOS CON MÁS DE UNA CATEGORIA ASIGNADA
#             La suma de importes de todos los movimientos es igual a
#             la suma de los totales de todas las categorías
#  (
#  CHECK <- near( sum(FLAG$Importe), sum(FLAG$Importe[FLAG$Comunidad==TRUE])+
#                                    sum(FLAG$Importe[FLAG$Telefono==TRUE])+
#                                    sum(FLAG$Importe[FLAG$Luz==TRUE])+
#                                    sum(FLAG$Importe[FLAG$Servicio==TRUE])+  
#                                    sum(FLAG$Importe[FLAG$Gasto_Corriente==TRUE])+
#                                    sum(FLAG$Importe[FLAG$Gasto_Otro==TRUE])+
#                                    sum(FLAG$Importe[FLAG$Transferencias==TRUE])+ 
#                                    sum(FLAG$Importe[FLAG$Check==TRUE])
#               )
#  )
#  remove(CHECK)
#
#             CHECK NO HAY MOVIMIENTOS CON MÁS DE UNA CATEGORIA ASIGNADA
#             La suma (lógica, 1 ó 0) de todos los campos categoría
#             Es precisamente 1 para cada movimiento
#  (
#  CHECK <- FLAG$Comunidad +
#           FLAG$Telefono +
#           FLAG$Luz +
#           FLAG$Servicio +
#           FLAG$Gasto_Corriente +
#           FLAG$Gasto_Otro +
#           FLAG$Transferencias== 1
#  )
#  CHECK[FALSE]  # No debe haber ningún FALSE
#  remove(CHECK)
#===============================================================================

                    # Definir variable Categoría segun el FILTRO que aplica
FLAG <- FLAG %>%
#Inicio de mutate
mutate(Categoria = case_when( #Inicio de case_when

  
# Comunidad
(grepl("CARGO DE RECIBOS", Descripcion)&
   (grepl("Recibo Cp Rfv", Concepto)|
    grepl("Recibo Raimundo Fernandez Villaverde", Concepto)|
    grepl("Recibo Geminis I, Garaje", Concepto)  
   )
)                                                              ~ "Comunidad",
  
# Telefono
(grepl("CARGO DE RECIBOS", Descripcion)&
   grepl("Recibo Yoigo", Concepto)
)                                                             ~ "Telefono",
  
#Luz
(grepl("CARGO DE RECIBOS", Descripcion)&
   (!grepl("Recibo Cp Rfv", Concepto)&
      !grepl("Recibo Raimundo Fernandez Villaverde", Concepto)&
      !grepl("Recibo Geminis I, Garaje", Concepto)&
      !grepl("Recibo Yoigo", Concepto)
   )
)                                                              ~ "Luz",        

# Servicio
(grepl("CARGO DE OPERACION CON TARJETA", Descripcion)&
 grepl("Reint", Concepto)&
 !abs(Importe) > 500
)|
(grepl("PAGO EN EFECTIVO", Descripcion)&
 grepl("Retirada De Efectivo En Cajero", Concepto)&
 abs(Importe) < 500 &
 abs(Importe) >= 370  
)                                                           ~ "Servicio",

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
  (grepl("PAGO EN EFECTIVO", Descripcion)&
#   grepl("Retirada De Efectivo En Cajero", Concepto)&
   abs(Importe) < 370  
  )
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
 )|
 (grepl("PAGO EN EFECTIVO", Descripcion)&
#    grepl("Retirada De Efectivo En Cajero", Concepto)&
    abs(Importe) >=500  
 )   
)                                                             ~ "Gasto_Otro", 




# Transferencias   
(grepl("TRANSFERENCIAS RECIBIDAS", Descripcion)&
   !abs(Importe) <= 200
)                                                           ~ "Transferencias",

# NC Movimiento sin Clasificar. No debe haber ninguno.
(TRUE)                                                        ~ "NC"                           
                            ) # Fin de case_when
      ) # Fin de mutate 

#================================================================================

#   #       CHECK no hay movimientos NC, sin variable de Categoría asignada
#   CHECK <- FLAG %>%
#            filter(Categoria == "NC") %>% 
#            summarise(num=n(),tot=sum(Importe))
#   CHECK$tot == 0
#   remove(CHECK)
#
# ========  HASTA AQUÍ PARA CLASIFICAR Y ETIQUETAR POR CATEGORÍAS ==============
