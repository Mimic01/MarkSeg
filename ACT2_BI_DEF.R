


#Script generador de reglas 
PATH="C:\\Users\\Alex\\Documents\\UAI\\BI\\ACT2_BI\\clientes.txt"
clientes=read.table(PATH, sep = " ")
library(arulesViz)
library(arules)
clientes<-clientes[,c(1,2,3,4,5,8,9,10,11,12,14,15)]
trx<-clientes


#NOMBRAMOS COLUMNAS

names(trx)=c("subcliente","Ncasas","Nhabitantes","edad","t.cliente","t.familia","educacion","trabajo","clase social","casa","salud","ingreso")

#DISCRETIZAMOS

trx<-as.data.frame(lapply(trx,function(x) discretize(x, categories=12)))

# CREAMOS REGLAS

reglas <- apriori(trx, parameter=list(support=0.1, confidence = 0.8))

# CANT DE REGLAS CREADAS

print(reglas)

# IMPRIME TODAS LAS REGLAS

inspect(reglas)

# IMPRIME LAS 5 REGLAS CON MAYOR CONFIANZA

reglas <-sort(reglas, by="confidence", decreasing=TRUE) # ordena regla 
inspect(head(reglas,5))
plot(reglas)

# IMPRIME LAS 5 REGLAS CON MAYOR LIFT

reglas.l <-sort(reglas, by="lift", decreasing=TRUE) # ordena regla 
inspect(head(reglas.l,5))
plot(reglas.l)
