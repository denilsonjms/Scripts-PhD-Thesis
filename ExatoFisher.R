if(!require(readxl)) install.packages("readxl")
library(readxl)
if(!require(GGally)) install.packages("GGally")
library(GGally)
library(ggplot2)


setwd("C:\\Users\\UFES\\Desktop\\Dados Oficiais para Tese")
getwd()


dim(Dados_IDeA_5ano)

diag_fun <- function(data, mapping, hist=list(), ...){
  
  X = eval_data_col(data, mapping$x)
  mn = mean(X)
  s = sd(X)
  
  ggplot(data, mapping) + 
    do.call(function(...) geom_histogram(aes(y =..density..), ...), hist) +
    stat_function(fun = dnorm, args = list(mean = mn, sd = s), ...)
}

#######################################
############### 5° Ano ################
#######################################

Dados_IDeA_5ano <- read_excel("Dados_IDeA_5ano.xlsx")

ggpairs(Dados_IDeA_5ano[ , 3:10],  diag = list(continuous = wrap(diag_fun, hist=list(fill="gray", colour="Blue"), 
                                                                 colour="Black", lwd=1)),
        lower = list(continuous = wrap("smooth", color="Blue", se=T)))  +theme_bw()


#######################################
############### 9° Ano ################
#######################################


Dados_IDeA_9ano <- read_excel("Dados_IDeA_9ano.xlsx")

ggpairs(Dados_IDeA_9ano[ , 3:10],  diag = list(continuous = wrap(diag_fun, hist=list(fill="gray", colour="Blue"), 
                                                                 colour="Black", lwd=1)),
        lower = list(continuous = wrap("smooth", color="Blue", se=T)))  +theme_bw()

summary(Dados_IDeA_9ano$Raça_Port)




alto_Port_5ano=subset(Dados_IDeA_5ano, Dados_IDeA_5ano$Português >= 4.8)
dim(alto_Port_5ano)
medioalto_Port_5ano=subset(Dados_IDeA_5ano, Dados_IDeA_5ano$Português < 4.8 & Dados_IDeA_5ano$Português >=3.8)
dim(medioalto_Port_5ano)
medio_Port_5ano=subset(Dados_IDeA_5ano, Dados_IDeA_5ano$Português < 3.8 & Dados_IDeA_5ano$Português >=2.9)
dim(medio_Port_5ano)
mediobaixo_Port_5ano=subset(Dados_IDeA_5ano, Dados_IDeA_5ano$Português < 2.9 & Dados_IDeA_5ano$Português >= 2)
dim(mediobaixo_Port_5ano)
baixo_Port_5ano=subset(Dados_IDeA_5ano, Dados_IDeA_5ano$Português < 2)
dim(baixo_Port_5ano)

alto_Mat_5ano=subset(Dados_IDeA_5ano, Dados_IDeA_5ano$Matemática >= 4.8)
dim(alto_Mat_5ano)
medioalto_Mat_5ano=subset(Dados_IDeA_5ano, Dados_IDeA_5ano$Matemática < 4.8 & Dados_IDeA_5ano$Matemática >=3.8)
dim(medioalto_Mat_5ano)
medio_Mat_5ano=subset(Dados_IDeA_5ano, Dados_IDeA_5ano$Matemática < 3.8 & Dados_IDeA_5ano$Matemática >=2.9)
dim(medio_Mat_5ano)
mediobaixo_Mat_5ano=subset(Dados_IDeA_5ano, Dados_IDeA_5ano$Matemática < 2.9 & Dados_IDeA_5ano$Matemática >= 2)
dim(mediobaixo_Mat_5ano)
baixo_Mat_5ano=subset(Dados_IDeA_5ano, Dados_IDeA_5ano$Matemática < 2)
dim(baixo_Mat_5ano)

alto_Port_9ano=subset(Dados_IDeA_9ano, Dados_IDeA_9ano$Português >= 4.8)
dim(alto_Port_9ano)
medioalto_Port_9ano=subset(Dados_IDeA_9ano, Dados_IDeA_9ano$Português < 4.8 & Dados_IDeA_9ano$Português >=3.8)
dim(medioalto_Port_9ano)
medio_Port_9ano=subset(Dados_IDeA_9ano, Dados_IDeA_9ano$Português < 3.8 & Dados_IDeA_9ano$Português >=2.9)
dim(medio_Port_9ano)
mediobaixo_Port_9ano=subset(Dados_IDeA_9ano, Dados_IDeA_9ano$Português < 2.9 & Dados_IDeA_9ano$Português >= 2)
dim(mediobaixo_Port_9ano)
baixo_Port_9ano=subset(Dados_IDeA_9ano, Dados_IDeA_9ano$Português < 2)
dim(baixo_Port_9ano)

alto_Mat_9ano=subset(Dados_IDeA_9ano, Dados_IDeA_9ano$Matemática >= 4.8)
dim(alto_Mat_9ano)
medioalto_Mat_9ano=subset(Dados_IDeA_9ano, Dados_IDeA_9ano$Matemática < 4.8 & Dados_IDeA_9ano$Matemática >=3.8)
dim(medioalto_Mat_9ano)
medio_Mat_9ano=subset(Dados_IDeA_9ano, Dados_IDeA_9ano$Matemática < 3.8 & Dados_IDeA_9ano$Matemática >=2.9)
dim(medio_Mat_9ano)
mediobaixo_Mat_9ano=subset(Dados_IDeA_9ano, Dados_IDeA_9ano$Matemática < 2.9 & Dados_IDeA_9ano$Matemática >= 2)
dim(mediobaixo_Mat_9ano)
baixo_Mat_9ano=subset(Dados_IDeA_9ano, Dados_IDeA_9ano$Matemática < 2)
dim(baixo_Mat_9ano)


##############################
########## Raça ##############
##############################


### Alto ###

equidade_port_alto = subset(alto_Port_9ano, alto_Port_9ano$Raça_Port >=-0.1)
dim(equidade_port_alto)
des_port_alto = subset(alto_Port_9ano, alto_Port_9ano$Raça_Port < -0.1 & alto_Port_9ano$Raça_Port >=-0.26)
dim(des_port_alto)
desAlt_port_alto = subset(alto_Port_9ano, alto_Port_9ano$Raça_Port <=-0.27)
dim(desAlt_port_alto)

equidade_Mat_alto = subset(alto_Mat_9ano, alto_Mat_9ano$Raça_Mat >=-0.1)
dim(equidade_Mat_alto)
des_Mat_alto = subset(alto_Mat_9ano, alto_Mat_9ano$Raça_Mat < -0.1 & alto_Mat_9ano$Raça_Mat >=-0.26)
dim(des_Mat_alto)
desAlt_Mat_alto = subset(alto_Mat_9ano, alto_Mat_9ano$Raça_Mat <=-0.27)
dim(desAlt_Mat_alto)


### Médio-Alto ###

equidade_port_medioalto = subset(medioalto_Port_9ano, medioalto_Port_9ano$Raça_Port >=-0.1)
dim(equidade_port_medioalto)
des_port_medioalto = subset(medioalto_Port_9ano, medioalto_Port_9ano$Raça_Port < -0.1 & medioalto_Port_9ano$Raça_Port >=-0.26)
dim(des_port_medioalto)
desAlt_port_medioalto = subset(medioalto_Port_9ano, medioalto_Port_9ano$Raça_Port <=-0.27)
dim(desAlt_port_medioalto)

equidade_Mat_medioalto = subset(medioalto_Mat_9ano, medioalto_Mat_9ano$Raça_Mat >=-0.1)
dim(equidade_Mat_medioalto)
des_Mat_medioalto = subset(medioalto_Mat_9ano, medioalto_Mat_9ano$Raça_Mat < -0.1 & medioalto_Mat_9ano$Raça_Mat >=-0.26)
dim(des_Mat_medioalto)
desAlt_Mat_medioalto = subset(medioalto_Mat_9ano, medioalto_Mat_9ano$Raça_Mat <=-0.27)
dim(desAlt_Mat_medioalto)


### Médio ###

equidade_port_medio = subset(medio_Port_9ano, medio_Port_9ano$Raça_Port >=-0.1)
dim(equidade_port_medio)
des_port_medio = subset(medio_Port_9ano, medio_Port_9ano$Raça_Port < -0.1 & medio_Port_9ano$Raça_Port >=-0.26)
dim(des_port_medio)
desAlt_port_medio = subset(medio_Port_9ano, medio_Port_9ano$Raça_Port <=-0.27)
dim(desAlt_port_medio)

equidade_Mat_medio = subset(medio_Mat_9ano, medio_Mat_9ano$Raça_Mat >=-0.1)
dim(equidade_Mat_medio)
des_Mat_medio = subset(medio_Mat_9ano, medio_Mat_9ano$Raça_Mat < -0.1 & medio_Mat_9ano$Raça_Mat >=-0.26)
dim(des_Mat_medio)
desAlt_Mat_medio = subset(medio_Mat_9ano, medio_Mat_9ano$Raça_Mat <=-0.27)
dim(desAlt_Mat_medio)


### Médio-baixo ###

equidade_port_mediobaixo = subset(mediobaixo_Port_9ano, mediobaixo_Port_9ano$Raça_Port >=-0.1)
dim(equidade_port_mediobaixo)
des_port_mediobaixo = subset(mediobaixo_Port_9ano, mediobaixo_Port_9ano$Raça_Port < -0.1 & mediobaixo_Port_9ano$Raça_Port >=-0.26)
dim(des_port_mediobaixo)
desAlt_port_mediobaixo = subset(mediobaixo_Port_9ano, mediobaixo_Port_9ano$Raça_Port <=-0.27)
dim(desAlt_port_mediobaixo)

equidade_Mat_mediobaixo = subset(mediobaixo_Mat_9ano, mediobaixo_Mat_9ano$Raça_Mat >=-0.1)
dim(equidade_Mat_mediobaixo)
des_Mat_mediobaixo = subset(mediobaixo_Mat_9ano, mediobaixo_Mat_9ano$Raça_Mat < -0.1 & mediobaixo_Mat_9ano$Raça_Mat >=-0.26)
dim(des_Mat_mediobaixo)
desAlt_Mat_mediobaixo = subset(mediobaixo_Mat_9ano, mediobaixo_Mat_9ano$Raça_Mat <=-0.27)
dim(desAlt_Mat_mediobaixo)


### Baixo ###

equidade_port_baixo = subset(baixo_Port_9ano, baixo_Port_9ano$Raça_Port >=-0.1)
dim(equidade_port_baixo)
des_port_baixo = subset(baixo_Port_9ano, baixo_Port_9ano$Raça_Port < -0.1 & baixo_Port_9ano$Raça_Port >=-0.26)
dim(des_port_baixo)
desAlt_port_baixo = subset(baixo_Port_9ano, baixo_Port_9ano$Raça_Port <=-0.27)
dim(desAlt_port_baixo)

equidade_Mat_baixo = subset(baixo_Mat_9ano, baixo_Mat_9ano$Raça_Mat >=-0.1)
dim(equidade_Mat_baixo)
des_Mat_baixo = subset(baixo_Mat_9ano, baixo_Mat_9ano$Raça_Mat < -0.1 & baixo_Mat_9ano$Raça_Mat >=-0.26)
dim(des_Mat_baixo)
desAlt_Mat_baixo = subset(baixo_Mat_9ano, baixo_Mat_9ano$Raça_Mat <=-0.27)
dim(desAlt_Mat_baixo)


##############################
########## NSE ##############
##############################


### Alto ###

equidade_port_alto = subset(alto_Port_9ano, alto_Port_9ano$NSE_Port >=-0.06)
dim(equidade_port_alto)
des_port_alto = subset(alto_Port_9ano, alto_Port_9ano$NSE_Port < -0.06 & alto_Port_9ano$NSE_Port >=-0.38)
dim(des_port_alto)
desAlt_port_alto = subset(alto_Port_9ano, alto_Port_9ano$NSE_Port <=-0.39)
dim(desAlt_port_alto)

equidade_Mat_alto = subset(alto_Mat_9ano, alto_Mat_9ano$NSE_MAT >=-0.06)
dim(equidade_Mat_alto)
des_Mat_alto = subset(alto_Mat_9ano, alto_Mat_9ano$NSE_MAT < -0.06 & alto_Mat_9ano$NSE_MAT >=-0.38)
dim(des_Mat_alto)
desAlt_Mat_alto = subset(alto_Mat_9ano, alto_Mat_9ano$NSE_MAT <=-0.39)
dim(desAlt_Mat_alto)


### Médio-Alto ###

equidade_port_medioalto = subset(medioalto_Port_9ano, medioalto_Port_9ano$NSE_Port >=-0.06)
dim(equidade_port_medioalto)
des_port_medioalto = subset(medioalto_Port_9ano, medioalto_Port_9ano$NSE_Port < -0.06 & medioalto_Port_9ano$NSE_Port >=-0.38)
dim(des_port_medioalto)
desAlt_port_medioalto = subset(medioalto_Port_9ano, medioalto_Port_9ano$NSE_Port <=-0.39)
dim(desAlt_port_medioalto)

equidade_Mat_medioalto = subset(medioalto_Mat_9ano, medioalto_Mat_9ano$NSE_MAT >=-0.06)
dim(equidade_Mat_medioalto)
des_Mat_medioalto = subset(medioalto_Mat_9ano, medioalto_Mat_9ano$NSE_MAT < -0.06 & medioalto_Mat_9ano$NSE_MAT >=-0.38)
dim(des_Mat_medioalto)
desAlt_Mat_medioalto = subset(medioalto_Mat_9ano, medioalto_Mat_9ano$NSE_MAT <=-0.39)
dim(desAlt_Mat_medioalto)


### Médio ###

equidade_port_medio = subset(medio_Port_9ano, medio_Port_9ano$NSE_Port >=-0.06)
dim(equidade_port_medio)
des_port_medio = subset(medio_Port_9ano, medio_Port_9ano$NSE_Port < -0.06 & medio_Port_9ano$NSE_Port >=-0.38)
dim(des_port_medio)
desAlt_port_medio = subset(medio_Port_9ano, medio_Port_9ano$NSE_Port <=-0.39)
dim(desAlt_port_medio)

equidade_Mat_medio = subset(medio_Mat_9ano, medio_Mat_9ano$NSE_MAT >=-0.06)
dim(equidade_Mat_medio)
des_Mat_medio = subset(medio_Mat_9ano, medio_Mat_9ano$NSE_MAT < -0.06 & medio_Mat_9ano$NSE_MAT >=-0.38)
dim(des_Mat_medio)
desAlt_Mat_medio = subset(medio_Mat_9ano, medio_Mat_9ano$NSE_MAT <=-0.39)
dim(desAlt_Mat_medio)


### Médio-baixo ###

equidade_port_mediobaixo = subset(mediobaixo_Port_9ano, mediobaixo_Port_9ano$NSE_Port >=-0.06)
dim(equidade_port_mediobaixo)
des_port_mediobaixo = subset(mediobaixo_Port_9ano, mediobaixo_Port_9ano$NSE_Port < -0.06 & mediobaixo_Port_9ano$NSE_Port >=-0.38)
dim(des_port_mediobaixo)
desAlt_port_mediobaixo = subset(mediobaixo_Port_9ano, mediobaixo_Port_9ano$NSE_Port <=-0.39)
dim(desAlt_port_mediobaixo)

equidade_Mat_mediobaixo = subset(mediobaixo_Mat_9ano, mediobaixo_Mat_9ano$NSE_MAT >=-0.06)
dim(equidade_Mat_mediobaixo)
des_Mat_mediobaixo = subset(mediobaixo_Mat_9ano, mediobaixo_Mat_9ano$NSE_MAT < -0.06 & mediobaixo_Mat_9ano$NSE_MAT >=-0.38)
dim(des_Mat_mediobaixo)
desAlt_Mat_mediobaixo = subset(mediobaixo_Mat_9ano, mediobaixo_Mat_9ano$NSE_MAT <=-0.39)
dim(desAlt_Mat_mediobaixo)


### Baixo ###

equidade_port_baixo = subset(baixo_Port_9ano, baixo_Port_9ano$NSE_Port >=-0.06)
dim(equidade_port_baixo)
des_port_baixo = subset(baixo_Port_9ano, baixo_Port_9ano$NSE_Port < -0.06 & baixo_Port_9ano$NSE_Port >=-0.38)
dim(des_port_baixo)
desAlt_port_baixo = subset(baixo_Port_9ano, baixo_Port_9ano$NSE_Port <=-0.39)
dim(desAlt_port_baixo)

equidade_Mat_baixo = subset(baixo_Mat_9ano, baixo_Mat_9ano$NSE_MAT >=-0.06)
dim(equidade_Mat_baixo)
des_Mat_baixo = subset(baixo_Mat_9ano, baixo_Mat_9ano$NSE_MAT < -0.06 & baixo_Mat_9ano$NSE_MAT >=-0.38)
dim(des_Mat_baixo)
desAlt_Mat_baixo = subset(baixo_Mat_9ano, baixo_Mat_9ano$NSE_MAT <=-0.39)
dim(desAlt_Mat_baixo)



M<-as.table(rbind(c(0,7,16,2,0),c(0,4,9,0,0),c(1,18,20,1,0)))
fisher.test(M)

N<-as.table(rbind(c(0,0,12,7,0),c(0,1,15,6,0),c(4,9,17,7,0)))
fisher.test(N)

O<-as.table(rbind(c(0,6,17,3,0),c(0,22,28,0,0),c(1,1,0,0,0)))
fisher.test(O)

P<-as.table(rbind(c(0,2,11,5,0),c(3,6,29,13,0),c(1,2,4,2,0)))
fisher.test(P)





M<-as.table(rbind(c(7,16,2),c(4,9,0),c(19,20,1)))
fisher.test(M)

N<-as.table(rbind(c(0,12,7),c(1,15,6),c(13,17,7)))
fisher.test(N)

O<-as.table(rbind(c(6,17,3),c(22,28,0),c(2,0,0)))
fisher.test(O)

P<-as.table(rbind(c(2,11,5),c(9,29,13),c(3,4,2)))
fisher.test(P)













































##############################
########## Raça ##############
##############################


### Alto ###

equidade_port_alto = subset(alto_Port_5ano, alto_Port_5ano$Raça_Port >=0.0)
dim(equidade_port_alto)
des_port_alto = subset(alto_Port_5ano, alto_Port_5ano$Raça_Port < 0.0 & alto_Port_5ano$Raça_Port >=-0.39)
dim(des_port_alto)
desAlt_port_alto = subset(alto_Port_5ano, alto_Port_5ano$Raça_Port <=-0.40)
dim(desAlt_port_alto)

equidade_Mat_alto = subset(alto_Mat_5ano, alto_Mat_5ano$Raça_Mat >=0.0)
dim(equidade_Mat_alto)
des_Mat_alto = subset(alto_Mat_5ano, alto_Mat_5ano$Raça_Mat < 0.0 & alto_Mat_5ano$Raça_Mat >=-0.39)
dim(des_Mat_alto)
desAlt_Mat_alto = subset(alto_Mat_5ano, alto_Mat_5ano$Raça_Mat <=-0.40)
dim(desAlt_Mat_alto)


### Médio-Alto ###

equidade_port_medioalto = subset(medioalto_Port_5ano, medioalto_Port_5ano$Raça_Port >=0.0)
dim(equidade_port_medioalto)
des_port_medioalto = subset(medioalto_Port_5ano, medioalto_Port_5ano$Raça_Port < 0.0 & medioalto_Port_5ano$Raça_Port >=-0.39)
dim(des_port_medioalto)
desAlt_port_medioalto = subset(medioalto_Port_5ano, medioalto_Port_5ano$Raça_Port <=-0.40)
dim(desAlt_port_medioalto)

equidade_Mat_medioalto = subset(medioalto_Mat_5ano, medioalto_Mat_5ano$Raça_Mat >=0.0)
dim(equidade_Mat_medioalto)
des_Mat_medioalto = subset(medioalto_Mat_5ano, medioalto_Mat_5ano$Raça_Mat < 0.0 & medioalto_Mat_5ano$Raça_Mat >=-0.39)
dim(des_Mat_medioalto)
desAlt_Mat_medioalto = subset(medioalto_Mat_5ano, medioalto_Mat_5ano$Raça_Mat <=-0.40)
dim(desAlt_Mat_medioalto)


### Médio ###

equidade_port_medio = subset(medio_Port_5ano, medio_Port_5ano$Raça_Port >=0.0)
dim(equidade_port_medio)
des_port_medio = subset(medio_Port_5ano, medio_Port_5ano$Raça_Port < 0.0 & medio_Port_5ano$Raça_Port >=-0.39)
dim(des_port_medio)
desAlt_port_medio = subset(medio_Port_5ano, medio_Port_5ano$Raça_Port <=-0.40)
dim(desAlt_port_medio)

equidade_Mat_medio = subset(medio_Mat_5ano, medio_Mat_5ano$Raça_Mat >=0.0)
dim(equidade_Mat_medio)
des_Mat_medio = subset(medio_Mat_5ano, medio_Mat_5ano$Raça_Mat < 0.0 & medio_Mat_5ano$Raça_Mat >=-0.39)
dim(des_Mat_medio)
desAlt_Mat_medio = subset(medio_Mat_5ano, medio_Mat_5ano$Raça_Mat <=-0.40)
dim(desAlt_Mat_medio)


### Médio-baixo ###

equidade_port_mediobaixo = subset(mediobaixo_Port_5ano, mediobaixo_Port_5ano$Raça_Port >=0.0)
dim(equidade_port_mediobaixo)
des_port_mediobaixo = subset(mediobaixo_Port_5ano, mediobaixo_Port_5ano$Raça_Port < 0.0 & mediobaixo_Port_5ano$Raça_Port >=-0.39)
dim(des_port_mediobaixo)
desAlt_port_mediobaixo = subset(mediobaixo_Port_5ano, mediobaixo_Port_5ano$Raça_Port <=-0.40)
dim(desAlt_port_mediobaixo)

equidade_Mat_mediobaixo = subset(mediobaixo_Mat_5ano, mediobaixo_Mat_5ano$Raça_Mat >=0.0)
dim(equidade_Mat_mediobaixo)
des_Mat_mediobaixo = subset(mediobaixo_Mat_5ano, mediobaixo_Mat_5ano$Raça_Mat < 0.0 & mediobaixo_Mat_5ano$Raça_Mat >=-0.39)
dim(des_Mat_mediobaixo)
desAlt_Mat_mediobaixo = subset(mediobaixo_Mat_5ano, mediobaixo_Mat_5ano$Raça_Mat <=-0.40)
dim(desAlt_Mat_mediobaixo)


### Baixo ###

equidade_port_baixo = subset(baixo_Port_5ano, baixo_Port_5ano$Raça_Port >=0.0)
dim(equidade_port_baixo)
des_port_baixo = subset(baixo_Port_5ano, baixo_Port_5ano$Raça_Port < 0.0 & baixo_Port_5ano$Raça_Port >=-0.39)
dim(des_port_baixo)
desAlt_port_baixo = subset(baixo_Port_5ano, baixo_Port_5ano$Raça_Port <=-0.40)
dim(desAlt_port_baixo)

equidade_Mat_baixo = subset(baixo_Mat_5ano, baixo_Mat_5ano$Raça_Mat >=0.0)
dim(equidade_Mat_baixo)
des_Mat_baixo = subset(baixo_Mat_5ano, baixo_Mat_5ano$Raça_Mat < 0.0 & baixo_Mat_5ano$Raça_Mat >=-0.39)
dim(des_Mat_baixo)
desAlt_Mat_baixo = subset(baixo_Mat_5ano, baixo_Mat_5ano$Raça_Mat <=-0.40)
dim(desAlt_Mat_baixo)


##############################
########## NSE ##############
##############################


### Alto ###

equidade_port_alto = subset(alto_Port_5ano, alto_Port_5ano$NSE_Port >=-0.1)
dim(equidade_port_alto)
des_port_alto = subset(alto_Port_5ano, alto_Port_5ano$NSE_Port < -0.1 & alto_Port_5ano$NSE_Port >=-0.38)
dim(des_port_alto)
desAlt_port_alto = subset(alto_Port_5ano, alto_Port_5ano$NSE_Port <=-0.39)
dim(desAlt_port_alto)

equidade_Mat_alto = subset(alto_Mat_5ano, alto_Mat_5ano$NSE_MAT >=-0.1)
dim(equidade_Mat_alto)
des_Mat_alto = subset(alto_Mat_5ano, alto_Mat_5ano$NSE_MAT < -0.1 & alto_Mat_5ano$NSE_MAT >=-0.38)
dim(des_Mat_alto)
desAlt_Mat_alto = subset(alto_Mat_5ano, alto_Mat_5ano$NSE_MAT <=-0.39)
dim(desAlt_Mat_alto)


### Médio-Alto ###

equidade_port_medioalto = subset(medioalto_Port_5ano, medioalto_Port_5ano$NSE_Port >=-0.1)
dim(equidade_port_medioalto)
des_port_medioalto = subset(medioalto_Port_5ano, medioalto_Port_5ano$NSE_Port < -0.1 & medioalto_Port_5ano$NSE_Port >=-0.38)
dim(des_port_medioalto)
desAlt_port_medioalto = subset(medioalto_Port_5ano, medioalto_Port_5ano$NSE_Port <=-0.39)
dim(desAlt_port_medioalto)

equidade_Mat_medioalto = subset(medioalto_Mat_5ano, medioalto_Mat_5ano$NSE_MAT >=-0.1)
dim(equidade_Mat_medioalto)
des_Mat_medioalto = subset(medioalto_Mat_5ano, medioalto_Mat_5ano$NSE_MAT < -0.1 & medioalto_Mat_5ano$NSE_MAT >=-0.38)
dim(des_Mat_medioalto)
desAlt_Mat_medioalto = subset(medioalto_Mat_5ano, medioalto_Mat_5ano$NSE_MAT <=-0.39)
dim(desAlt_Mat_medioalto)


### Médio ###

equidade_port_medio = subset(medio_Port_5ano, medio_Port_5ano$NSE_Port >=-0.1)
dim(equidade_port_medio)
des_port_medio = subset(medio_Port_5ano, medio_Port_5ano$NSE_Port < -0.1 & medio_Port_5ano$NSE_Port >=-0.38)
dim(des_port_medio)
desAlt_port_medio = subset(medio_Port_5ano, medio_Port_5ano$NSE_Port <=-0.39)
dim(desAlt_port_medio)

equidade_Mat_medio = subset(medio_Mat_5ano, medio_Mat_5ano$NSE_MAT >=-0.1)
dim(equidade_Mat_medio)
des_Mat_medio = subset(medio_Mat_5ano, medio_Mat_5ano$NSE_MAT < -0.1 & medio_Mat_5ano$NSE_MAT >=-0.38)
dim(des_Mat_medio)
desAlt_Mat_medio = subset(medio_Mat_5ano, medio_Mat_5ano$NSE_MAT <=-0.39)
dim(desAlt_Mat_medio)


### Médio-baixo ###

equidade_port_mediobaixo = subset(mediobaixo_Port_5ano, mediobaixo_Port_5ano$NSE_Port >=-0.1)
dim(equidade_port_mediobaixo)
des_port_mediobaixo = subset(mediobaixo_Port_5ano, mediobaixo_Port_5ano$NSE_Port < -0.1 & mediobaixo_Port_5ano$NSE_Port >=-0.38)
dim(des_port_mediobaixo)
desAlt_port_mediobaixo = subset(mediobaixo_Port_5ano, mediobaixo_Port_5ano$NSE_Port <=-0.39)
dim(desAlt_port_mediobaixo)

equidade_Mat_mediobaixo = subset(mediobaixo_Mat_5ano, mediobaixo_Mat_5ano$NSE_MAT >=-0.1)
dim(equidade_Mat_mediobaixo)
des_Mat_mediobaixo = subset(mediobaixo_Mat_5ano, mediobaixo_Mat_5ano$NSE_MAT < -0.1 & mediobaixo_Mat_5ano$NSE_MAT >=-0.38)
dim(des_Mat_mediobaixo)
desAlt_Mat_mediobaixo = subset(mediobaixo_Mat_5ano, mediobaixo_Mat_5ano$NSE_MAT <=-0.39)
dim(desAlt_Mat_mediobaixo)


### Baixo ###

equidade_port_baixo = subset(baixo_Port_5ano, baixo_Port_5ano$NSE_Port >=-0.1)
dim(equidade_port_baixo)
des_port_baixo = subset(baixo_Port_5ano, baixo_Port_5ano$NSE_Port < -0.1 & baixo_Port_5ano$NSE_Port >=-0.38)
dim(des_port_baixo)
desAlt_port_baixo = subset(baixo_Port_5ano, baixo_Port_5ano$NSE_Port <=-0.39)
dim(desAlt_port_baixo)

equidade_Mat_baixo = subset(baixo_Mat_5ano, baixo_Mat_5ano$NSE_MAT >=-0.1)
dim(equidade_Mat_baixo)
des_Mat_baixo = subset(baixo_Mat_5ano, baixo_Mat_5ano$NSE_MAT < -0.1 & baixo_Mat_5ano$NSE_MAT >=-0.38)
dim(des_Mat_baixo)
desAlt_Mat_baixo = subset(baixo_Mat_5ano, baixo_Mat_5ano$NSE_MAT <=-0.39)
dim(desAlt_Mat_baixo)



M<-as.table(rbind(c(0,2,3,0,0),c(8,32,7,0,0),c(17,6,2,1,0)))
fisher.test(M)

N<-as.table(rbind(c(0,1,6,0,0),c(6,17,20,2,0),c(10,11,4,1,0)))
fisher.test(N)

O<-as.table(rbind(c(0,8,4,0,0),c(11,26,5,0,0),c(14,6,3,1,0)))
fisher.test(O)

P<-as.table(rbind(c(0,5,5,0,0),c(7,17,17,3,0),c(9,7,8,0,0)))
fisher.test(P)





M<-as.table(rbind(c(2,3,0),c(40,7,0),c(23,2,1)))
fisher.test(M)

N<-as.table(rbind(c(1,6,0),c(23,20,2),c(21,4,1)))
fisher.test(N)

O<-as.table(rbind(c(8,4,0),c(37,5,0),c(20,3,1)))
fisher.test(O)

P<-as.table(rbind(c(5,5,0),c(24,17,3),c(16,8,0)))
fisher.test(P)







