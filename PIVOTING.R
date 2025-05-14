source("REPORT_ING.R")

#  TRASPONER ESTE DATA FRAME 
TO_EXCEL <- pivot_longer(REPORT,cols = (-Fecha_Final), names_to="Concepto") %>% 
            pivot_wider(names_from=c(Fecha_Final))

#==============================================================================

#           TRASPONIENDO REPORT (PIVOTING LONGER AND THEN WIDER)

# EN 1 PASO
TO_EXCEL <- REPORT
TO_EXCEL <- pivot_longer(TO_EXCEL, cols = (-Fecha_Final), names_to="name") %>% 
            pivot_wider(names_from=c(Fecha_Final))

# EN 2 PASOS
# Trasponer
TO_EXCEL <- REPORT
TO_EXCEL <- pivot_longer(TO_EXCEL, cols = (-Fecha_Final))#, names_to="name") %>%  
TO_EXCEL <- pivot_wider(TO_EXCEL, names_from=c(Fecha_Final))

# EN 2 PASOS
# Hacer el cambio y luego deshacerlo
TO_EXCEL <- REPORT
TO_EXCEL <- pivot_longer(TO_EXCEL, cols = (-Fecha_Final))#, names_to="name") %>%
TO_EXCEL <- pivot_wider(TO_EXCEL, names_from="name")

#==============================================================================

Fechas10 = unique(REPORT$Fecha_Final)[1:10]

MITIBBLE=tibble(Fechas10, x=1:10, y=3*x, z=10*x)
View(MITIBBLE)

# longer everything() except Fechas10
NEWMITIBBLE1 <- pivot_longer(MITIBBLE,cols = (-Fechas10), names_to="Concepto") #%>% 
                #pivot_wider(names_from=c(Fechas10))
View(NEWMITIBBLE1)

# longer then wider by Fechas10
NEWMITIBBLE2 <- pivot_longer(MITIBBLE,cols = (-Fechas10), names_to="Concepto") %>% 
                pivot_wider(names_from=c(Fechas10))
View(NEWMITIBBLE2)

# longer then wider by Concepto
NEWMITIBBLE3 <- pivot_longer(MITIBBLE,cols = (-Fechas10), names_to="Concepto") %>% 
                pivot_wider(names_from=c(Concepto))
View(NEWMITIBBLE3)


# Sin Fechas

MITIBBLE=tibble(x=1:10, y=3*x, z=10*x)
View(MITIBBLE)

# longer everything() except Fechas10
NEWMITIBBLE1 <- pivot_longer(MITIBBLE,everything(), names_to="Concepto") #%>% 
                #pivot_wider(names_from=c(Fechas10))
View(NEWMITIBBLE1)

# longer then wider by Fechas10
NEWMITIBBLE2 <- pivot_longer(MITIBBLE,everything(), names_to="Concepto") %>% 
                pivot_wider(names_from=c(value))
View(NEWMITIBBLE2)

# longer then wider by Concepto
NEWMITIBBLE3 <- pivot_longer(MITIBBLE,everything(), names_to="Concepto") %>% 
  pivot_wider(names_from=c(Concepto))
View(NEWMITIBBLE3)



