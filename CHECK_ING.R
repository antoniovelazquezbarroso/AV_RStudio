source("FLAG_ING.R")

#===============================================================================
  
  #       CHECK no hay movimientos Check, sin variable de Categoría asignada
  CHECK <- FLAG %>% summarise(num=n(),tot=sum(Importe[Check]))
  CHECK$tot == 0
  remove(CHECK)
   
  #       CHECK NO HAY MOVIMIENTOS CON MÁS DE UNA CATEGORIA ASIGNADA
  #       La suma de importes de todos los movimientos es igual a
  #       la suma de los totales de todas las categorías
  (
  CHECK <- near(sum(FLAG$Importe), sum(FLAG$Importe[FLAG$Nomina==TRUE])+
                                    sum(FLAG$Importe[FLAG$Casa==TRUE])+
                                    sum(FLAG$Importe[FLAG$Recibo_Cte==TRUE])+
                                    sum(FLAG$Importe[FLAG$Recibo_Otr==TRUE])+
                                    sum(FLAG$Importe[FLAG$Gasto_Cte==TRUE])+
                                    sum(FLAG$Importe[FLAG$Gasto_Otr==TRUE])+
                                    sum(FLAG$Importe[FLAG$Patrimonio==TRUE])+
                                    sum(FLAG$Importe[FLAG$Check==TRUE]))
  )
  remove(CHECK)
  
  #          La suma de todas las Categorías (binarias) de un Mov es 1
  (
  CHECK <- FLAG$Nomina +
           FLAG$Casa +
           FLAG$Recibo_Cte +
           FLAG$Recibo_Otr +
           FLAG$Gasto_Cte +
           FLAG$Gasto_Otr +
           FLAG$Patrimonio == 1
  )
  CHECK[FALSE]    # No existe ninguno FALSE, en todos suma precisamente 1
  remove(CHECK)
#===============================================================================
