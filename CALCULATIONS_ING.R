#   source("LOAD_ING.R")
#   source("FLAG_ING.R")
#   # source("CHECK_ING.R") # ¿AÑADIRLE UN MENSAJE PARA CASO DE ERRORES?
source("REPORT_ING.R")
source("MyFunctions.R")

#===============================================================================
#  CUADRO DE ESTADÍSTICAS SOBRE COLUMNAS DE TOTALES SELECCIONADAS DE REPORT

MISCOLS <- REPORT %>% select(Casa:Recibos)

MISCOLS %>% pivot_longer(everything()) %>% 
            group_by(name) %>% 
            summarise(
                      num=length(value),
                      #n_miss = sum(is.na(value)),
                      mean=mean(-value),
                      sd=sd(-value),
                      #skew <- sum(-value-mean)^3/sd^3/num,
                      #kurt <- sum(-value-mean)^4/sd^4/num - 3,
                      min=min(-value),
                      q1=quantile(-value, prob=0.25),
                      med=median(-value),
                      q3=quantile(-value, prob=0.75),
                      max=max(-value)
                     )

#===============================================================================
#  CALCULANDO NUEVOS VALORES A PARTIR DE LOS TOTALES, 
#  PARA INFORMES ADHOC, O PARA AÑADIRLOS AL REPORT

#    Mis funciones predefinidas
source("MyFunctions.R")

# Valores con lags en el tiempo
REPORT$Gasto_Cte
( Anterior <- lag(REPORT$Gasto_Cte) )


#    Añadiendo campos calculados para un informe y gráfico ad-hoc
# PARA Gasto_Cte
ADHOC <- REPORT %>% mutate(LAG=lag(Gasto_Cte),
                           MEAN=mean(Gasto_Cte),
                           MiMM3=MM3(Gasto_Cte),
                           MiMM4=MM4(Gasto_Cte),
                           MiMM12=MMnum(Gasto_Cte, 12)
) %>% 
  select(Fecha_Final, Gasto_Cte, LAG, MEAN, MiMM3, MiMM12, Total_Gasto) %>% 
  filter(Fecha_Final>"2022-12-31")%>%
  arrange(rev(Fecha_Final)) %>% 
  print(n=nrow(REPORT)) 

ggplot(ADHOC, aes(Fecha_Final, -Gasto_Cte)) +
  geom_col(fill="LIGHTGREEN")+
  geom_line(aes(Fecha_Final, -MEAN), colour="BLUE")+
  #  geom_line(aes(Fecha_Final, -LAG), colour="BLUE")+
  geom_line(aes(Fecha_Final, -MiMM3), colour="RED")+
  geom_line(aes(Fecha_Final, -MiMM12), colour="YELLOW")+
#  geom_point(aes(Fecha_Final, -LAG))
  geom_segment(aes(x = Fecha_Final, y = 0, xend = Fecha_Final, yend = -Total_Gasto))+
  geom_point(aes(x = Fecha_Final, -Total_Gasto))

# PARA Total_Gasto
ADHOC <- REPORT %>% mutate(LAG=lag(Total_Gasto),
                           MEAN=mean(REPORT$Total_Gasto),
                           MiMM3=MM3(Total_Gasto),
                           MiMM4=MM4(Total_Gasto),
                           MiMM12=MMnum(Total_Gasto, 12)
                          ) %>% 
         select(Fecha_Final, Total_Gasto, LAG, MEAN, MiMM3, MiMM4, MiMM12) %>% 
         filter(Fecha_Final>"2022-12-31") %>%
         arrange(rev(Fecha_Final)) %>% 
         print(n=nrow(REPORT))

ggplot(ADHOC, aes(Fecha_Final, -Total_Gasto)) +
  geom_col(fill="LIGHTGREEN")+
  geom_line(aes(Fecha_Final, -MEAN), colour="BLUE")+
  geom_line(aes(Fecha_Final, -MiMM3), colour="RED")+
  geom_line(aes(Fecha_Final, -MiMM12), colour="YELLOW")


#   Añadiendo campos calculados al propio REPORT

REPORT <- REPORT %>% mutate(G_Cte_MM3=MM3(Gasto_Cte))

REPORT <- REPORT %>% mutate(G_Cte_MM3=MM3(Gasto_Cte),
                            Total_Gasto_MM3=MM3(Total_Gasto)
)

#===============================================================================

# Valores calculados por columna individual de REPORT
summary(REPORT$Total_Gasto)
length(REPORT$Total_Gasto)
sum(REPORT$Total_Gasto)
mean(REPORT$Total_Gasto) # Promedia los totales de todos los periodos
sd(REPORT$Total_Gasto)   # Desviación típica entre los de todos los periodos

summary(REPORT$Gasto_Cte)
length(REPORT$Gasto_Cte)
sum(REPORT$Gasto_Cte)
mean(REPORT$Gasto_Cte) # Promedia los totales de todos los periodos
sd(REPORT$Gasto_Cte)   # Desviación típica entre los de todos los periodos

summary(REPORT$Gasto_Otr)
length(REPORT$Gasto_Otr)
sum(REPORT$Gasto_Otr)
mean(REPORT$Gasto_Otr)  # Promedia los totales de todos los periodos
sd(REPORT$Gasto_Otr)    # Desviación típica entre los de todos los periodos


#summary(abs(REPORT$Total_Gasto))
summary(abs(REPORT$Total_Gasto))
length(abs(REPORT$Total_Gasto))  # n
sum(is.na(REPORT$Total_Gasto))   # n_miss
sum(abs(REPORT$Total_Gasto))
mean(abs(REPORT$Total_Gasto)) # Promedia los totales de todos los periodos
sd(abs(REPORT$Total_Gasto))
max(abs(REPORT$Total_Gasto))
min(abs(REPORT$Total_Gasto))
median(abs(REPORT$Total_Gasto))
quantile(abs(REPORT$Total_Gasto), prob=0.25)
quantile(abs(REPORT$Total_Gasto), prob=0.75)
quantile(abs(REPORT$Total_Gasto), prob=0.00)
quantile(abs(REPORT$Total_Gasto), prob=1.00)
MM3(abs(REPORT$Total_Gasto))
MM4(abs(REPORT$Total_Gasto))


#summary(abs(REPORT$Gasto_Cte))
summary(abs(REPORT$Gasto_Cte))
length(abs(REPORT$Gasto_Cte))   # n
sum(is.na(REPORT$Gasto_Cte))   # n_miss
sum(-REPORT$Gasto_Cte)
mean(-REPORT$Gasto_Cte) # Promedia los totales de todos los periodos
sd(abs(REPORT$Gasto_Cte))
max(abs(REPORT$Gasto_Cte))
min(abs(REPORT$Gasto_Cte))
median(abs(REPORT$Gasto_Cte))
quantile(abs(REPORT$Gasto_Cte), prob=0.25)
quantile(abs(REPORT$Gasto_Cte), prob=0.75)
quantile(abs(REPORT$Gasto_Cte), prob=0.00)
quantile(abs(REPORT$Gasto_Cte), prob=1.00)
MM3(abs(REPORT$Gasto_Cte))
MM4(abs(REPORT$Gasto_Cte))