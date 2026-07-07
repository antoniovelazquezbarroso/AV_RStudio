source("LOAD_BS.R")

FLAG <- BS
#===============================================================================
                               #  Eliminar columnas innecesarias por simplificar 
FLAG <- FLAG %>% select(- NumOrden, - Saldo)  # Antes también quitaba -Codigo

#===============================================================================
                         # Definir variables FILTRO por Categoría de Movimientos
FLAG <- FLAG %>%
  #Inicio de mutate
  mutate(
         # Recibos Comunidad Casa y Garaje
         Comunidad=(grepl("Recibo Cp Rfv", Concepto)|
                    grepl("Recibo Raimundo Fernandez Villaverde", Concepto)|
                    grepl("Recibo Geminis I, Garaje", Concepto)  
                   ),
         
         # Recibos Telefono
         Telefono=(grepl("Recibo Yoigo", Concepto)|
                   grepl("Recibo Grupo Masmovil" , Concepto)
                  ),
         
         # Recibos Luz
         Luz=(grepl("Recibo Repsol", Concepto)|
              grepl("Recibo Naturgy", Concepto)|
              grepl("Recibo Gesternova", Concepto)|
              grepl("Recibo Endesa", Concepto)
             ),            

         # Servicio 
                    # Reintegros Cajero hasta €500 para pagar Pilar
                    # Los mayores a €500 van a Gasto_Otro
         Servicio=(
                   (grepl("Reint", Concepto)|
                    grepl("Retirada De Efectivo En Cajero", Concepto)
                   ) &  
                   abs(Importe) <= 550 &
                   abs(Importe) >= 200  
                  ),         
         
         # Gasto_Corriente 
                    # Cargos Tarjeta excepto Cajeros y hasta €200      
                    # Transferecias emitidas hasta €200
                    # Transferencias recibidas hasta €200 (compensacion gastos)
                    # Conceptos Atípicos (devoluciones,compensacion gastos)
                    # Cajeros hasta €200 (no es Servicio)         
            
         Gasto_Corriente=(
           
                    # Cargos Tarjeta excepto Cajeros y hasta €200 
           (
            (grepl("Compra", Concepto)|
             grepl("Pago", Concepto)|
             grepl("Transaccion", Concepto)|
             grepl("Liquidacion", Concepto)  # 2 Raros
            ) &
            abs(Importe) <= 200
           )|
                   # Transferencias emitidas hasta €200
           (                      
             (grepl("Transferencia A", Concepto)|
              grepl("Traspaso", Concepto)|
              grepl("Bizum", Concepto)
             ) &
             abs(Importe) <= 200
           )|
                   # Transferencias recibidas hasta €200 (es compensacion gasto)             
           (grepl("Transferencia De", Concepto) &
            abs(Importe) <= 200
           )|
                   # Conceptos Atípicos (devoluciones,compensacion gasto)             
           (        
            (grepl("Devolucion", Concepto)|
             grepl("Anulacion", Concepto)
            ) &
            abs(Importe) <= 200  # Nunca los hubo mayores
           )|
                   # Cajeros hasta €200 (no es Servicio)             
           (       
            (grepl("Reint", Concepto)|
             grepl("Retirada De Efectivo En Cajero", Concepto)
            ) & 
            abs(Importe) < 200 
           )
                        ),

         # Gasto_Otro         
           # Cargos Tarjeta excepto Cajeros y mayores de €200       
           # Transferencias Emitidas mayores de €200 (menores van a Tarjeta_Cte)
           # Cajeros mayores de €550 (los menores van a Servicio)
         Gasto_Otro=(
           
           # Cargos Tarjeta excepto Cajeros y mayores de €200 
           (
             (grepl("Compra", Concepto)|
                grepl("Pago", Concepto)|
                grepl("Transaccion", Concepto)
             ) &
               abs(Importe) > 200
           )|
               # Transferencias emitidas  mayores a €200 
           (                     
            (grepl("Transferencia A", Concepto)|
             grepl("Traspaso", Concepto)|
             grepl("Bizum", Concepto)
            ) &
            abs(Importe) > 200
           )|
              # Cajeros mayor que €550 (no es Servicio)     
           (                         
            (grepl("Reint", Concepto)| 
             grepl("Retirada De Efectivo En Cajero", Concepto)
            ) &  
            abs(Importe) > 550 
           )
 
                   ),
         # Transferencias
                      # Transferencias a casa (excluyendo menores de € 200)
                      # las menores van a Gasto_Cte (por compensación gastos)
                                                          
         Transferencias=(grepl("Transferencia De", Concepto)&
                         !abs(Importe) <= 200 &
                         !grepl("Pago Renta 2023", Concepto) # CASO RARO
    # Eva, por error pago el 8nov2024 su IRPF desde Cta Casa (aparece como Compra Internet)
    # Lo compensó con una transferencia a la casa. Esta se clasifica como Gasto_Otro porque
    # en el concepto aparece "Pago" y es mayor de 220; así se compensa por el gasto).
    # No debe, además contarse como Transferencia para Gastos Casa
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
mutate(
       Categoria = case_when( #Inicio de case_when

  
# Comunidad
(
  grepl("Recibo Cp Rfv", Concepto)|
  grepl("Recibo Raimundo Fernandez Villaverde", Concepto)|
  grepl("Recibo Geminis I, Garaje", Concepto) 
)                                                              ~ "Comunidad",

# Telefono
(
  grepl("Recibo Yoigo", Concepto)|
  grepl("Recibo MasMovil", Concepto) # Cuando venga
)                                                             ~ "Telefono",

#Luz
(
  grepl("Recibo Repsol", Concepto)|
  grepl("Recibo Naturgy", Concepto)|
  grepl("Recibo Gesternova", Concepto)|
  grepl("Recibo Endesa", Concepto)
)                                                              ~ "Luz",        

# Servicio
(
  (grepl("Reint", Concepto)|
   grepl("Retirada De Efectivo En Cajero", Concepto)
  ) &  
  abs(Importe) <= 550 &
  abs(Importe) >= 200
)                                                           ~ "Servicio",

# Gasto_Corriente  

# Cargos Tarjeta excepto Cajeros y hasta €200      
# Transferecias emitidas hasta €200
# Transferencias recibidas hasta €200 (compensacion gastos)
# Conceptos Atípicos (devoluciones,compensacion gastos)
# Cajeros hasta €200 (no es Servicio) 
(
  # Cargos Tarjeta excepto Cajeros y hasta €200 
 (
  (grepl("Compra", Concepto)|
   grepl("Pago", Concepto)|
   grepl("Transaccion", Concepto)|
   grepl("Liquidacion", Concepto)  # 2 Raros
  ) &
  abs(Importe) <= 200
 )|
  # Transferencias emitidas hasta €200
 (                      
   (grepl("Transferencia A", Concepto)|
    grepl("Traspaso", Concepto)|
    grepl("Bizum", Concepto)
   ) &
   abs(Importe) <= 200
 )|
 # Transferencias recibidas hasta €200 (es compensacion gasto)             
 (grepl("Transferencia De", Concepto) &
  abs(Importe) <= 200
 )|
 # Conceptos Atípicos (devoluciones,compensacion gasto)             
 (        
  (grepl("Devolucion", Concepto)|
   grepl("Anulacion", Concepto)
  ) &
  abs(Importe) <= 200  # Nunca los hubo mayores
 )|
 # Cajeros hasta €200 (no es Servicio)             
 (       
  (grepl("Reint", Concepto)|
   grepl("Retirada De Efectivo En Cajero", Concepto)
  ) & 
  abs(Importe) < 200 
 )
)                                                          ~ "Gasto_Corriente",  

# Gasto_Otro

  # Cargos Tarjeta excepto Cajeros y mayores de €200       
  # Transferencias Emitidas mayores de €200 (menores van a Tarjeta_Cte)
  # Cajeros mayores de €550 (los menores van a Servicio)
(
  # Cargos Tarjeta excepto Cajeros y mayores de €200 
 (
  (grepl("Compra", Concepto)|
   grepl("Pago", Concepto)|
   grepl("Transaccion", Concepto)
  ) &
  abs(Importe) > 200
 )|
  # Transferencias emitidas  mayores a €200 
 (                     
  (grepl("Transferencia A", Concepto)|
   grepl("Traspaso", Concepto)|
   grepl("Bizum", Concepto)
  ) &
  abs(Importe) > 200
 )|
  # Cajeros mayor que €550 (no es Servicio)     
 (                         
  (grepl("Reint", Concepto)| 
     grepl("Retirada De Efectivo En Cajero", Concepto)
  ) &  
  abs(Importe) > 550 
 )
)                                                             ~ "Gasto_Otro", 

# Transferencias 

  # Transferencias a casa (excluyendo menores de € 200)
  # las menores van a Gasto_Cte (por compensación gastos)
(
  grepl("Transferencia De", Concepto)&
    !abs(Importe) <= 200  &                         
    !grepl("Pago Renta 2023", Concepto) # CASO RARO
  # Eva, por error pago el 8nov2024 su IRPF desde Cta Casa (aparece como Compra Internet)
  # Lo compensó con una transferencia a la casa. Esta se clasifica como Gasto_Otro porque
  # en el concepto aparece "Pago" y es mayor de 220; así se compensa por el gasto).
  # No debe, además contarse como Transferencia para Gastos Casa
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
