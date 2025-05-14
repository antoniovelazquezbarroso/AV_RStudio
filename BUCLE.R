source("REPORT_ING.R")
source("MyFunctions.R")

MISCOLS <- REPORT %>% select(Recibo_Cte:Total_Fijo)

names(MISCOLS)
dim(MISCOLS)
length(MISCOLS)
ncol(MISCOLS)
nrow(MISCOLS)
str(MISCOLS)
summary(MISCOLS)

MISCOLS                 # tibble 42 x 10 (nrow x ncol)
MISCOLS[1]              # tibble 42 x 1 
MISCOLS[1:3]            # tibble 42 x 3
MISCOLS[1,3]            # tibble 1 x 1
MISCOLS[3,1]            # tibble 1 x 1
MISCOLS[[1,3]]          # vector, length 1
MISCOLS[1,]             # tibble 1 x ncol
MISCOLS[,1]             # tibble nrow x 1
MISCOLS["Casa"]         # tibble 42 x 1
MISCOLS[1]              # tibble 42 x 1

MISCOLS$Casa            # vector
MISCOLS[[1]]            # vector
as.vector(MISCOLS[1])   # vector


MyStats(MISCOLS$Casa)
MyStats( (MISCOLS[1]) ) # no funciona
MyStats( (MISCOLS[[1]]) )


MISCOLS %>% summarize(
                      across(Recibo_Cte:Total_Fijo, mean)
                     )

MISCOLS %>% summarize(
                      across(Recibo_Cte:Total_Fijo, summary)
                     )

MISCOLS %>% summarize(
                      across(Recibo_Cte:Total_Fijo, MyStats)
                     )

MISCOLS %>% pivot_longer(everything()) %>% #
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

MISCOLS %>% pivot_longer(everything()) 
