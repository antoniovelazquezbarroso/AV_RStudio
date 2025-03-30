library(tidyverse)

source("LOAD_ING.R")

ll <-  ING

ll <-  ING %>% 
       arrange(desc(Importe)) # Todos, por importe de mayor a menor

ll <-  ING %>% 
       arrange(Descripcion)   # Todos, por Descripcion (Básico para clasificar)

#=============================================================================
#                                               # NÓMINAS
#
ll <- ING %>%                                               # Nóminas  (Nomina)
      filter(grepl("Nomina recibida", Descripcion))

#                                               # TRASPASOS A/DE CTA NARANJA
#
ll <- ING %>%                       # Traspasos a/de cuenta naranja (Patrimonio)
      filter(grepl("Traspaso emitido Cuenta Nómina", Descripcion)|
             grepl("Traspaso recibido Cuenta Nómina", Descripcion)
             ) 

#  ll <- ING %>%                  # Eliminando traspasos a/de cuenta naranja
#        filter(!grepl("Traspaso emitido Cuenta Nómina", Descripcion)&
#               !grepl("Traspaso recibido Cuenta Nómina", Descripcion)
#              ) 

#                                                # TRANSFERENCIAS EMITIDAS
#
#  ll <- ING %>%                    # Transferencias emitidas (todas)
#        filter(grepl("Transferencia emitida", Descripcion))


#                                                                 (Patrimonio)
ll <- ING %>%                      # Transferencias emitidas -   (Suscrip_BdE)
  filter(grepl("Transferencia emitida", Descripcion)&
           grepl("Suscrip", Descripcion) 
  )

#  ll <- ING %>%                  # Transferencias emitidas (sin Suscrip_BdE)
#        filter(grepl("Transferencia emitida", Descripcion)&
#               !grepl("Suscrip", Descripcion) 
#              )
#
#  ll <- ING %>%                  # Transferencias periódicas para Casa
#        filter(grepl("Transferencia emitida periódica", Descripcion)
#              )
#  
#  
#  ll <- ING %>%                  # Transferencias a Eva para Casa
#        filter(grepl("Transferencia emitida a EVA", Descripcion)|
#               grepl("Transferencia emitida a Eva", Descripcion)
#              ) 

ll <- ING %>%                 #  Transferencias a Casa Todas -            (Casa) 
  filter(grepl("Transferencia emitida periódica", Descripcion)|
         grepl("Transferencia emitida a EVA", Descripcion)|
         grepl("Transferencia emitida a Eva", Descripcion)
        )

#  ll <- ING %>%       # Otras Transferencias (no a casa, ni BdE)
#        filter(grepl("Transferencia emitida", Descripcion)&
#               !grepl("Transferencia emitida periódica", Descripcion)&
#               !grepl("Transferencia emitida a EVA", Descripcion)&
#               !grepl("Transferencia emitida a Eva", Descripcion)&
#               !grepl("Suscrip", Descripcion)
#              ) 

# Otras Transferencias (no a casa, ni BdE)
ll <- ING %>%     # Importe > 1.000                                 (Patrimonio)
  filter(grepl("Transferencia emitida", Descripcion)&
           !grepl("Transferencia emitida periódica", Descripcion)&
           !grepl("Transferencia emitida a EVA", Descripcion)&
           !grepl("Transferencia emitida a Eva", Descripcion)&
           !grepl("Suscrip", Descripcion)&
           abs(Importe) > 1000
        )

# Otras Transferencias (no a casa, ni BdE)
ll <- ING %>%      # Importe > 200 e Importe <=1.000               (a Gasto_Otr)
  filter(grepl("Transferencia emitida", Descripcion)&
         !grepl("Transferencia emitida periódica", Descripcion)&
         !grepl("Transferencia emitida a EVA", Descripcion)&
         !grepl("Transferencia emitida a Eva", Descripcion)&
         !grepl("Suscrip", Descripcion)&
         abs(Importe) > 200 & abs(Importe) <=1000
        )

# Otras Transferencias (no a Eva, ni BdE)
ll <- ING %>%      # Importe <= 200                                (a Gasto_Cte)
  filter(grepl("Transferencia emitida", Descripcion)&
         !grepl("Transferencia emitida periódica", Descripcion)&
         !grepl("Transferencia emitida a EVA", Descripcion)&
         !grepl("Transferencia emitida a Eva", Descripcion)&
         !grepl("Suscrip", Descripcion)&
         abs(Importe) <= 200
        )

#                                                # TRANSFERENCIAS RECIBIDAS
#
#   ll <- ING %>%                 # Transferencias recibidas (todas)
#         filter(grepl("Transferencia recibida", Descripcion))
                                                                  # (Patrimonio)
ll <- ING %>%             # Transferencia recibida de BANCO DE ESPA - (Int_BdE)
      filter(grepl("Transferencia recibida", Descripcion)&
             grepl("Transferencia recibida de BANCO DE ESPA", Descripcion) 
            ) 

#  ll <- ING %>%                  # Transferencias recibidas (excl. Int_BdE)
#        filter(grepl("Transferencia recibida", Descripcion)&
#               !grepl("Transferencia recibida de BANCO DE ESPA", Descripcion) 
#              ) 

                              # Transferencias recibidas (excl. Int_BdE)
ll <- ING %>%                 # con Importe >= 532                # (Patrimonio)
      filter(grepl("Transferencia recibida", Descripcion)&
             !grepl("Transferencia recibida de BANCO DE ESPA", Descripcion)&
             Importe >= 532   # Por excluir transf Eva 3-feb-2022
            )

                              # Transferencias recibidas (excl. Int_BdE)
ll <- ING %>%                 # Importe < 532       (x compensación - Gasto_Cte)
      filter(grepl("Transferencia recibida", Descripcion)&
               !grepl("Transferencia recibida de BANCO DE ESPA", Descripcion)&
               !Importe >= 532    # Por incluir transf Eva 3-feb-2022
            )

#                                                     # RECIBOS
#
#  ll <- ING %>%                                            #  Recibos (todos)
#        filter(grepl("Recibo", Descripcion))

                    # Recibos Pago Anual                           (Recibos_Otr)
ll <- ING %>%
  filter(grepl("Recibo", Descripcion)&
         (grepl("AYUNTAMIENTO", Descripcion)|
          grepl("Ayuntamiento", Descripcion)|
          grepl("MUTUA", Descripcion)
         )
        ) 

                    # Recibos Pago Mensual(excl. anuales)          (Recibos_Cte)
ll <- ING %>% 
  filter(grepl("Recibo", Descripcion)&
         !(grepl("AYUNTAMIENTO", Descripcion)|
           grepl("Ayuntamiento", Descripcion)|
           grepl("MUTUA", Descripcion)
          )
        )
#                                                     #  REINTEGRO

#  ll <- ING %>%                                       #  Reintegros (todos)
#        filter(grepl("Reintegro", Descripcion)) 

                                                                #  (a Gasto_Cte)
ll <- ING %>%                                          #  Reintegros <= 500
      filter(grepl("Reintegro", Descripcion)&
             Importe <= 500  
            ) 
#                                                                  (a Gasto_Otr)
ll <- ING %>%                                 # Reintegros > 500 y <= 1.000
      filter(grepl("Reintegro", Descripcion)&
             Importe > 500  & Importe <= 1000
            ) 
#                                                                 (a Patrimonio)
ll <- ING %>%                                       #  Reintegros > 1.000
      filter(grepl("Reintegro", Descripcion)&
             !Importe <= 1000
            )

#                                                    #  PAGO
          
#  ll <- ING %>%                                        #  Pagos (todos)
#        filter(grepl("Pago", Descripcion))

#                                                                  (a Gasto_Cte)
ll <- ING %>%                                                 #  Pagos <= 250
      filter(grepl("Pago", Descripcion)&
             Importe <= 250  
            ) 
#                                                                  (a Gasto_Otr)
ll <- ING %>%                                           # Pagos > 250 y <= 2.000
      filter(grepl("Pago", Descripcion)&
               Importe > 250  & Importe <= 2000
            ) 
#                                                                 (a Patrimonio)
ll <- ING %>%                                            # Pagos > 2.000
      filter(grepl("Pago", Descripcion)&
             !Importe <= 2000
            )

#                                  #   OTRAS DESCRIPCIONES NO INCLUIDAS
#
#
#  ll <- ING %>% filter(                                             
#                       (!grepl("Pago", Descripcion)&                 
#                        !grepl("Reintegro", Descripcion)&
#                        !grepl("Recibo", Descripcion)&
#                        !grepl("Transferencia", Descripcion)&
#                        !grepl("Traspaso", Descripcion)&
#                        !grepl("Nomina", Descripcion)
#                        )|
#                        (grepl("Traspaso interno emitido", Descripcion)
#                        )
#                       )
#  

                                              # Si >= 1000          (Patrimonio)
ll <- ING %>% filter(
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
                    )


                                # Si < 1000  ( x compensacion - Gasto_Corriente)
ll <- ING %>% filter(
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
                    )
                    

#===============================================================================

ll <- ING %>%
      mutate(
             T_Periodica=grepl("Transferencia emitida periódica", Descripcion),
             
             T_Eva_Casa=grepl("De Antonio para Casa", Descripcion)|
                        grepl("De Antonio para casa", Descripcion)|
                        grepl("De Antonio para CASA", Descripcion)|
                        grepl("De Antonio para Gastos Casa", Descripcion),
             
             T_Casa=T_Periodica|T_Eva_Casa,
             
             T_Eva_Otras=(
                          grepl("Transferencia emitida a EVA", Descripcion)&
                          !grepl("De Antonio para Casa", Descripcion)&
                          !grepl("De Antonio para casa", Descripcion)&
                          !grepl("De Antonio para CASA", Descripcion)&
                          !grepl("De Antonio para Gastos Casa", Descripcion)
                         ),
             
             T_Otras = (
                        grepl("Transferencia emitida", Descripcion)&
                        !grepl("Transferencia emitida periódica", Descripcion)&
                        !grepl("Transferencia emitida a EVA", Descripcion)&
                        !grepl("Suscrip", Descripcion)
                       ),

             Traspaso = (
                         grepl("Traspaso emitido Cuenta Nómina", Descripcion)|
                         grepl("Traspaso recibido Cuenta Nómina", Descripcion)
                        ),
             
             Suscrip_BdE = grepl("Suscrip", Descripcion),
             
             Interes_BdE = grepl("Transferencia recibida de BANCO DE ESPA", Descripcion),
             
             Nomina = grepl("Nomina recibida", Descripcion)
             ) %>%
      select(-NumOrden,-Saldo, -Categoria,-Subcategoria)
      filter(TRUE)
    
                                       # Totaliza por meses las categorias filtradas
 
ll %>% group_by(Año = year(Fecha), Mes = month(Fecha)) %>%
       summarise(num=n(),
                 nomina=sum(Importe[Nomina]),
                 t_casa=sum(Importe[T_Casa]),
                 t_otras=sum(Importe[T_Otras]),
                 interes=sum(Importe[Interes_BdE])
                ) %>% 
       print(n=nrow(ll))
      
#===============================================================================  
