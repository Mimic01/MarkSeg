#Nominal(Tipos):
 # 1,4,5
#Ordinal (leve, moderado, fuerte.):
 # 6,7,8,9,10,11,12,13,14,15
#Cuantitativa:
 # 2,3

library(arules)
library(arulesViz)
PATH="C:\\Users\\Alex\\Documents\\UAI\\BI\\ACT2_BI\\clientes.txt"
clientes=read.table(PATH, sep = " ")
clientes<-clientes[,-c(16:60)] # eliminamos el resto de las variables
clientes_cuant<-clientes[,-c(1,4,5,6,7,8,9,10,11,12,13,14,15)]
clientes_nom<-clientes[,-c(6,7,8,9,10,11,12,13,14,15,2,3)]
clientes_ord<-clientes[,-c(1,4,5,2,3)]

s_clientes_cuant<-scale(clientes_cuant)
s_clientes_nom<-scale(clientes_nom)
s_clientes_ord<-scale(clientes_ord)

library(caret)
cor_clientes_cuant<-cor(s_clientes_cuant)
findCorrelation(cor_clientes_cuant, cutoff = 0.8, verbose = FALSE, names = FALSE, exact = ncol(clientes) < 100)
#como no hay que eliminar ninguna variable numerica hay que volver a definir clientes con las nominales correspondientes

#count(clientes, 'V1')  solo da frecuencia de variable 1

library(Hmisc)
#Histogramas variables ordinales
hist.data.frame(clientes_ord)
#Histogramas variables nominales
hist.data.frame(clientes_nom)

#Redefinimos eliminando V6, V7 y V13
clientes<-clientes[,-c(6,7,13)]

#discretize multiple columns
#for (i in 1:12){clientes[,i]<-discretize(clientes[,i])}



#Desde aqui redefinimos clientes y empezamos las reglas de asociacion
#Script generador de reglas 
PATH="C:\\Users\\Alex\\Documents\\UAI\\BI\\ACT2_BI\\clientes.txt"
clientes=read.table(PATH, sep = " ")
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



