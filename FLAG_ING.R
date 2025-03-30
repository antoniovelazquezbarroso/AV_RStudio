source("LOAD_ING.R")
FLAG <- ING

#===============================================================================
                                           #  Eliminar columnas innecesarias
           #  Por simplificar, además luego crearemos una variable Categoria 
FLAG <- FLAG %>% select(- NumOrden, -Saldo, -Categoria, -Subcategoria)

#===============================================================================
                        # Definir variables FILTRO por Categoría de Movimientos
FLAG <- FLAG %>%
       #Inicio de mutate
       mutate(

Nomina = grepl("Nomina recibida", Descripcion),

Casa = grepl("Transferencia emitida periódica", Descripcion)|
       grepl("Transferencia emitida a EVA", Descripcion)|
       grepl("Transferencia emitida a Eva", Descripcion),

Recibo_Cte = (grepl("Recibo", Descripcion)&              # Recibos Pago Mensual
              !(grepl("AYUNTAMIENTO", Descripcion)|
                grepl("Ayuntamiento", Descripcion)|
                grepl("MUTUA", Descripcion)
               )
             ),

Recibo_Otr = (grepl("Recibo", Descripcion)&                # Recibos Pago Anual
              (grepl("AYUNTAMIENTO", Descripcion)|
               grepl("Ayuntamiento", Descripcion)|
               grepl("MUTUA", Descripcion)
              )
             ),

                                                                    # Gasto_Cte
Gasto_Cte = (grepl("Pago", Descripcion)&                         #  Pagos <= 250
             abs(Importe) <= 250)|
            (grepl("Reintegro", Descripcion)&                # Reintegros <= 500
             abs(Importe) <= 500)| 
            (grepl("Transferencia emitida", Descripcion)&   # Transf_Emit <= 200
             !grepl("Transferencia emitida periódica", Descripcion)&
             !grepl("Transferencia emitida a EVA", Descripcion)&
             !grepl("Transferencia emitida a Eva", Descripcion)&
             !grepl("Suscrip", Descripcion)&
             abs(Importe) <= 200)|
            (grepl("Transferencia recibida", Descripcion)&   # Transf_Reci < 532
             !grepl("Transferencia recibida de BANCO DE ESPA", Descripcion)&
             abs(Importe) <= 532)|  
            (                                             # Otros-raros <= 1.000
              ( 
                (!grepl("Pago", Descripcion)&                 
                 !grepl("Reintegro", Descripcion)&
                 !grepl("Recibo", Descripcion)&
                 !grepl("Transferencia", Descripcion)&
                 !grepl("Traspaso", Descripcion)&
                 !grepl("Nomina", Descripcion)
                )|
                (grepl("Traspaso interno emitido", Descripcion)
                )
              )&
              abs(Importe) <= 1000
            ),

                                                                    # Gasto_Otr
Gasto_Otr = (grepl("Pago", Descripcion))&               # Pagos > 250 y <= 2.000
             abs(Importe) > 250 &
             abs(Importe) <= 2000 &
             !grepl("Transferencia emitida a Eva Abascal Esteban Mueble Lavabo Rodrisan Segundo Pago", Descripcion)|
            (grepl("Reintegro", Descripcion)&      # Reintegros > 500 y <= 1.000
             abs(Importe) > 500 & abs(Importe) <= 1000)| 
            (grepl("Transferencia emitida", Descripcion)&  # Transf_Emit > 200 y
             (abs(Importe) > 200 & abs(Importe) <= 1000)&
             !grepl("Transferencia emitida periódica", Descripcion)&
             !grepl("Transferencia emitida a EVA", Descripcion)&
             !grepl("Transferencia emitida a Eva", Descripcion)&
             !grepl("Suscrip", Descripcion)&
             abs(Importe) > 200 & abs(Importe) <= 1000
            ),
                                                                   # Patrimonio
Patrimonio = (grepl("Pago", Descripcion)&                       #  Pagos > 2.000
              !abs(Importe) <= 2000)|
             (grepl("Reintegro", Descripcion)&              # Reintegros > 1.000
              !abs(Importe) <= 1000)| 
             (grepl("Transferencia emitida", Descripcion)& # Transf_Emit > 1.000
              !grepl("Transferencia emitida periódica", Descripcion)&
              !grepl("Transferencia emitida a EVA", Descripcion)&
              !grepl("Transferencia emitida a Eva", Descripcion)&
              !grepl("Suscrip", Descripcion)&
              !abs(Importe) <= 1000)|
             (grepl("Transferencia recibida", Descripcion)&  # Transf_Reci > 532
              !grepl("Transferencia recibida de BANCO DE ESPA", Descripcion)&
              !abs(Importe) <= 532)|  
             (                                             # Otros-raros > 1.000
              ( 
               (!grepl("Pago", Descripcion)&                 
                !grepl("Reintegro", Descripcion)&
                !grepl("Recibo", Descripcion)&
                !grepl("Transferencia", Descripcion)&
                !grepl("Traspaso", Descripcion)&
                !grepl("Nomina", Descripcion)
               )|
               (grepl("Traspaso interno emitido", Descripcion)
               )
              )&
              !abs(Importe) <= 1000
             )|
             (grepl("Traspaso emitido Cuenta Nómina", Descripcion)|  # Traspasos
              grepl("Traspaso recibido Cuenta Nómina", Descripcion)  
             )|
             (grepl("Transferencia emitida", Descripcion)&         # Suscrip_Bde
              grepl("Suscrip", Descripcion) 
             )|
             (grepl("Transferencia recibida", Descripcion)&            # Int_Bde
              grepl("Transferencia recibida de BANCO DE ESPA", Descripcion) 
             ), 

#       NO DEBE HABER MOVIMIENTOS SIN CLASIFICAR                      # Check
Check=(
       !Nomina &
       !Casa &
       !Recibo_Cte &
       !Recibo_Otr &
       !Gasto_Cte &
       !Gasto_Otr &
       !Patrimonio
      )
             ) # Fin de mutate          
         
#===============================================================================
#  
#  #       CHECK no hay movimientos Check, sin variable de Categoría asignada
#  CHECK <- FLAG %>% summarise(num=n(),tot=sum(Importe[Check]))
#  CHECK$tot == 0
#  remove(CHECK)
#   
#  #       CHECK NO HAY MOVIMIENTOS CON MÁS DE UNA CATEGORIA ASIGNADA
#  #       La suma de importes de todos los movimientos es igual a
#  #       la suma de los totales de todas las categorías
#  (
#  CHECK <- near(sum(FLAG$Importe), sum(FLAG$Importe[FLAG$Nomina==TRUE])+
#                                    sum(FLAG$Importe[FLAG$Casa==TRUE])+
#                                    sum(FLAG$Importe[FLAG$Recibo_Cte==TRUE])+
#                                    sum(FLAG$Importe[FLAG$Recibo_Otr==TRUE])+
#                                    sum(FLAG$Importe[FLAG$Gasto_Cte==TRUE])+
#                                    sum(FLAG$Importe[FLAG$Gasto_Otr==TRUE])+
#                                    sum(FLAG$Importe[FLAG$Patrimonio==TRUE])+
#                                    sum(FLAG$Importe[FLAG$Check==TRUE]))
#  )
#  remove(CHECK)
#  
#  (
#  CHECK <- FLAG$Nomina +
#           FLAG$Casa +
#           FLAG$Recibo_Cte +
#           FLAG$Recibo_Otr +
#           FLAG$Gasto_Cte +
#           FLAG$Gasto_Otr +
#           FLAG$Patrimonio == 1
#  )
#  CHECK[FALSE]
#  remove(CHECK)

#===============================================================================
                      # Definir variable Categoría segun el FILTRO que aplica
FLAG <- FLAG %>%
  #Inicio de mutate
  mutate(Categoria = case_when( #Inicio de case_when

# Nomina -----------------------------------------------------------------------
grepl("Nomina recibida", Descripcion)                           ~ "Nomina",

# Casa -------------------------------------------------------------------------
grepl("Transferencia emitida periódica", Descripcion)|
grepl("Transferencia emitida a EVA", Descripcion)|
grepl("Transferencia emitida a Eva", Descripcion)               ~ "Casa",

# Recibos Pago Mensual ---------------------------------------------------------
(grepl("Recibo", Descripcion)&               
 !(grepl("AYUNTAMIENTO", Descripcion)|
     grepl("Ayuntamiento", Descripcion)|
     grepl("MUTUA", Descripcion)
 )
)                                                               ~ "Recibo_Cte",                                                    

# Recibos Pago Anual -----------------------------------------------------------
(grepl("Recibo", Descripcion)&                 
 (grepl("AYUNTAMIENTO", Descripcion)|
  grepl("Ayuntamiento", Descripcion)|
  grepl("MUTUA", Descripcion)
 )
)                                                               ~ "Recibo_Otr",

# Gasto_Cte --------------------------------------------------------------------
(grepl("Pago", Descripcion)&                         #  Pagos <= 250
 abs(Importe) <= 250)|
(grepl("Reintegro", Descripcion)&                # Reintegros <= 500
 abs(Importe) <= 500)| 
(grepl("Transferencia emitida", Descripcion)&   # Transf_Emit <= 200
 !grepl("Transferencia emitida periódica", Descripcion)&
 !grepl("Transferencia emitida a EVA", Descripcion)&
 !grepl("Transferencia emitida a Eva", Descripcion)&
 !grepl("Suscrip", Descripcion)&
 abs(Importe) <= 200)|
(grepl("Transferencia recibida", Descripcion)&   # Transf_Reci < 532
 !grepl("Transferencia recibida de BANCO DE ESPA", Descripcion)&
 abs(Importe) <= 532)|  
(                                             # Otros-raros <= 1.000
 ( 
  (!grepl("Pago", Descripcion)&                 
   !grepl("Reintegro", Descripcion)&
   !grepl("Recibo", Descripcion)&
   !grepl("Transferencia", Descripcion)&
   !grepl("Traspaso", Descripcion)&
   !grepl("Nomina", Descripcion)
  )|
  (grepl("Traspaso interno emitido", Descripcion)
  )
 )&
 abs(Importe) <= 1000
)                                                               ~ "Gasto_Cte",
                                                                
# Gasto_Otr --------------------------------------------------------------------                                                                 
(grepl("Pago", Descripcion)&                # Pagos > 250 y <= 2.000
 abs(Importe) > 250 & abs(Importe) <= 2000)|
(grepl("Reintegro", Descripcion)&      # Reintegros > 500 y <= 1.000
 abs(Importe) > 500 & abs(Importe <= 1000))| 
(grepl("Transferencia emitida", Descripcion)&  # Transf_Emit > 200 y
 !grepl("Transferencia emitida periódica", Descripcion)& #  <= 1.000
 !grepl("Transferencia emitida a EVA", Descripcion)&
 !grepl("Transferencia emitida a Eva", Descripcion)&
 !grepl("Suscrip", Descripcion)&
 abs(Importe) > 200 & abs(Importe) <= 1000)                     ~ "Gasto_Otr",                                                              
                                                                
#  "Patrimonio" ----------------------------------------------------------------                                                                
(grepl("Pago", Descripcion)&                       #  Pagos > 2.000
 !abs(Importe) <= 2000)|
(grepl("Reintegro", Descripcion)&              # Reintegros > 1.000
 !abs(Importe) <= 1000)| 
(grepl("Transferencia emitida", Descripcion)& # Transf_Emit > 1.000
 !grepl("Transferencia emitida periódica", Descripcion)&
 !grepl("Transferencia emitida a EVA", Descripcion)&
 !grepl("Transferencia emitida a Eva", Descripcion)&
 !grepl("Suscrip", Descripcion)&
 !abs(Importe) <= 1000)|
(grepl("Transferencia recibida", Descripcion)&  # Transf_Reci > 532
 !grepl("Transferencia recibida de BANCO DE ESPA", Descripcion)&
 !abs(Importe) <= 532)|  
(                                             # Otros-raros > 1.000
 ( 
  (!grepl("Pago", Descripcion)&                 
   !grepl("Reintegro", Descripcion)&
   !grepl("Recibo", Descripcion)&
   !grepl("Transferencia", Descripcion)&
   !grepl("Traspaso", Descripcion)&
   !grepl("Nomina", Descripcion)
  )|
  (grepl("Traspaso interno emitido", Descripcion)
  )
 )&
 !abs(Importe) <= 1000
)|
(grepl("Traspaso emitido Cuenta Nómina", Descripcion)|  # Traspasos
 grepl("Traspaso recibido Cuenta Nómina", Descripcion)  
)|
(grepl("Transferencia emitida", Descripcion)&         # Suscrip_Bde
 grepl("Suscrip", Descripcion) 
)|
(grepl("Transferencia recibida", Descripcion)&            # Int_Bde
 grepl("Transferencia recibida de BANCO DE ESPA", Descripcion) 
)                                                               ~ "Patrimonio",                                                              

# NC, No debe haber movimientos sin clasificar ---------------------------------
(TRUE)                                                          ~ "NC"                           
                               )# Fin de case_when
         )# Fin de mutate                                                         
                                                        
#===============================================================================
#    #       CHECK no hay movimientos NC, sin variable de Categoría asignada
# CHECK <- FLAG %>%
#          filter(Categoria == "NC") %>% 
#          summarise(num=n(),tot=sum(Importe))
# (CHECK$tot == 0)
# remove(CHECK)
# 
#    # Todas las Categorías, por su orden default
# Cat=sort(unique(FLAG$Categoria))
# Cat
# # [1] "Casa"       "Gasto_Cte"  "Gasto_Otr"  "Nomina"     "Patrimonio" "Recibo_Cte" "Recibo_Otr"
# #     "Recibo_Cte" "Recibo_Otr"
# remove(Cat)
#
# #       CHECK NO HAY MOVIMIENTOS CON MÁS DE UNA CATEGORIA ASIGNADA
# #       La suma de importes de todos los movimientos es igual a
# #       la suma de los totales de todas las categorías
# (
# CHECK <- near(sum(FLAG$Importe), 
#               (sum(FLAG$Importe[FLAG$Categoria == "Nomina"])+
#                sum(FLAG$Importe[FLAG$Categoria == "Casa"])+
#                sum(FLAG$Importe[FLAG$Categoria == "Recibo_Cte"])+
#                sum(FLAG$Importe[FLAG$Categoria == "Recibo_Otr"])+
#                sum(FLAG$Importe[FLAG$Categoria == "Gasto_Cte"])+
#                sum(FLAG$Importe[FLAG$Categoria == "Gasto_Otr"])+
#                sum(FLAG$Importe[FLAG$Categoria == "Patrimonio"])
#               )
#              )
# )
# remove(CHECK)
# 
# ========  HASTA AQUÍ PARA CLASIFICAR Y ETIQUETAR POR CATEGORÍAS ==============
