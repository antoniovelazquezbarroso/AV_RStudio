# source("FLAG_ING.R")
# source("CHECK_ING.R") # ¿AÑADIRLE UN MENSAJE PARA CASO DE ERRORES?
# source("REPORT_ING.R")

#  # Valores con lags en el tiempo
#  REPORT$Gasto_Cte
#  ( Anterior <- lag(REPORT$Gasto_Cte) )


# Funciones media movil de los 3 , 4  ó  num últimos periodos
MM3 <- function(MiVar){
 (MiVar+lag(MiVar, n=1)+lag(MiVar, n=2))/3
} # Promedia totales de los 3 últimos periodos (el ultimo trimestre, para meses)
#    # Ejemplo uso MM3
#    MM3(REPORT$Gasto_Cte) # Promedia los totales de los 3 últimos periodos

MM4 <- function(MiVar){
 (MiVar+lag(MiVar, n=1)+lag(MiVar, n=2)+lag(MiVar, n=3))/4
} # Promedia totales de los 4 últimos periodos (el ultimo año, para trimestres)

MMnum <- function(MiVar, num){
  MMnum=vector( typeof(MiVar), length = length(MiVar))          
  for (i in seq_along(MiVar)){
    if (i < num ) MMnum[i]=NA else MMnum[i]=  sum(MiVar[ (i - num + 1) : i])/num
  }
  return(MMnum[1:length(MiVar)]) 
}
#  MMnum(REPORT$Total_Gasto, 12)
  
  
#  Funciones resumen estadístico de una variable
MyStats <- function(MiVar){mean=mean(-MiVar)
                           num=length(MiVar)
                           sd=sd(MiVar)
                           skew <- sum(MiVar-mean)^3/sd^3/num
                           kurt <- sum(MiVar-mean)^4/sd^4/num - 3
                           min=min(-MiVar)
                           q1=quantile(-MiVar, prob=0.25)
                           med=median(-MiVar)
                           q3=quantile(-MiVar, prob=0.75)
                           max=max(-MiVar)
           return(c(mean, num, sd, skew, kurt, min, q1,med, q3, max))
}
# MyStats(REPORT$Gasto_Cte)


summary6 <- function(data, var) {
  data |> summarize(
    min = min({{ var }}, na.rm = TRUE),
    q1=quantile({{ var }}, prob=0.25),
    mean = mean({{ var }}, na.rm = TRUE),
    median = median({{ var }}, na.rm = TRUE),
    q3=quantile({{ var }}, prob=0.75),
    max = max({{ var }}, na.rm = TRUE),
    n = n(),
    n_miss = sum(is.na({{ var }})),
    sd=sd({{ var }}),
    skew = sum({{ var }}-mean)^3/sd^3/n,
    kurt = sum({{ var }}-mean)^4/sd^4/n - 3,  
    .groups = "drop"
  )
}
#   summary6(REPORT, Gasto_Cte)
