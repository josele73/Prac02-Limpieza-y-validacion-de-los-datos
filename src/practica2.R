#install.packages("dplyr")
#install.packages('ggplot2')
#install.packages('plotrix')
#install.packages('psych')
library(dplyr)
library(arules)
library(arulesViz)
library(ggplot2)
library(car)
library(RcmdrMisc)
library(rgdal)
library(plotrix)
library(psych)


#Indicamos el directorio de los ficherros
setwd('C:/R/PRAC2TIP')

#leemos los ficheros
anyo2015<-read.csv('2015_accidents.csv',sep=",",header=TRUE,dec = ",")

#las variables
names(anyo2015)

#Eliminamos las columnas
#anyo2015<-subset(anyo2015,select = -c(3,5,7))

#tipo columna 6
class(anyo2015[,6])
#
str (anyo2015)
#
summary(anyo2015)
#Comprobamos los valores nulos
summarise_all(anyo2015, funs(sum(is.na(.))))


#3.2 Identificación y tratamiento de valores extremos

#Descripcion dia de la semana
print("Valores columna Descripcio.dia.setmana")
table(anyo2015$Descripció.dia.setmana)

#Descripcion tipo de dia
print("Valores columna Descripcio.tipus.dia")
table(anyo2015$Descripció.tipus.dia)

#Descripcion del mes
print("Valores columna mes.de.any")
table(anyo2015$Mes.de.any)

#Valores dia mes
range(anyo2015$Dia.de.mes,na.rm=TRUE)
table(anyo2015$Dia.de.mes)

#hora del dia
range(anyo2015$Hora.de.dia,na.rm=TRUE)
table(anyo2015$Hora.de.dia)

#Descipcion causa vianant
table(anyo2015$Descripció.causa.vianant)

#Descripcion tipo vehiculos
table(anyo2015$Desc..Tipus.vehicle.implicat)

#Descripcion sexo
table(anyo2015$Descripció.sexe)

#Descipcion de los tipos de personas
table(anyo2015$Descripció.tipus.persona)

#Valores edad
table(anyo2015$Edat)

#Descipcion victimas
table(anyo2015$Descripció.victimització)

#coordenadas UTM XY
range(X)


#4.1 Análisis temporal de los accidentes.


#Heridos en cada mes
table(anyo2015$Descripció.victimització,anyo2015$Nom.mes)
prop.table(table(anyo2015$Descripció.victimització,anyo2015$Nom.mes),1)
plot(anyo2015$Nom.mes,main='Heridos en cada mes',col='3',ylab='Total heridos',xlab='Meses del año')

# Heridos dia del mes
table(anyo2015$Descripció.victimització,anyo2015$Dia.de.mes)
prop.table(table(anyo2015$Descripció.victimització,anyo2015$Dia.de.mes),1)
diames<-c(table(unlist((anyo2015$Dia.de.mes))))
barplot(diames,col='4',main='Heridos en cada dia de mes',ylab='Total heridos',xlab='Dias del mes')

#Heridos en cada dia de la semana
table(anyo2015$Descripció.victimització,anyo2015$Dia.setmana)
prop.table(table(anyo2015$Descripció.victimització,anyo2015$Dia.setmana),1)
dias<-c(table(unlist((anyo2015$Dia.setmana))))
barplot(dias,col='5',main='Heridos en cada dia de semana',ylab='Total heridos',xlab='Dias de la semana')
dias
orden<-c(3,7,4,1,2,6,5)
diasord<-data.frame(dias,orden)

####################FALTA#################

#Accidentes a lo largo del año
#accidentesaños<-data.frame(anyo2015$Dia.de.mes,anyo2015$Mes.de.any)
#sustituimos mes por totoal dias
#accidentesaños$anyo2015.Mes.de.any<- revalue(accidentesaños$anyo2015.Mes.de.any, c(2=31))
#accidentesaños<-data.frame(anyo2015$Dia.de.mes,anyo2015$Mes.de.any)
#fechacompleta<-c(paste(as.character(anyo2015$Dia.de.mes),"/",as.character(anyo2015$Mes.de.any),"/2015"))
#fecha<-c(table(unlist((fechacompleta))))
#plot(fecha)


####################FALTA#################




# 4.2 Analisis de social personas heridas
#Eliminamos valores desconocidos
anyo2015Edat <- anyo2015[anyo2015$Edat != 'Desconegut', ]


#Convertimos de factor a numerico
anyo2015Edat$Edat<-as.numeric(anyo2015Edat$Edat)

#Edad Maxima
max(anyo2015Edat$Edat)
#edad minima
min(anyo2015Edat$Edat)
#Histograma edad
hist(anyo2015Edat$Edat,main='Edad de los heridos',col='15',ylab='Frequencia',xlab='Edad')
#Frequencias edad y sexo
edadsexo= data.frame(anyo2015Edat$Edat,anyo2015Edat$Descripció.sexe)
summary(edadsexo)
#Desviacion por edad y sexo
aggregate(anyo2015Edat$Edat,by=list(anyo2015Edat$Descripció.sexe),mean,na.rm=TRUE)
boxplot(anyo2015Edat$Edat~anyo2015Edat$Descripció.sexe)

#Relacion edat y heridas
table(anyo2015$Descripció.victimització,anyo2015$Edat)

#relacion sexo y heridas
table(anyo2015$Descripció.victimització,anyo2015$Descripció.sexe)

#segun heridas
valores<-c(table(unlist((anyo2015$Descripció.victimització))))
etiqueta<-c("Graves","Leves","Muertos")
etiqueta<-paste(etiqueta,valores)
pie3D(table(anyo2015$Descripció.victimització))
pie(valores,etiqueta, col=rainbow(3),main='Tipos y cantidad de heridos')


#barplot(table(anyo2015$Edat),xlab="Region",main="Happiness level by region", col=rainbow(10))

#4.3 Analisis geoespacial

anyo2015$Coordenada.UTM..X.<-as.numeric(anyo2015$Coordenada.UTM..X.)
anyo2015$Coordenada.UTM..Y.<-as.numeric(anyo2015$Coordenada.UTM..Y.)
coor<-anyo2015[anyo2015$Coordenada.UTM..X. != -1, ]
write.csv(coor, file="2015_coordenadas.csv")

#plot(anyo2015$Coordenada.UTM..Y.,anyo2015$Coordenada.UTM..X.)

anyo2015Coordenadas <- anyo2015[anyo2015$Coordenada.UTM..X. > 0, ]
anyo2015Coordenadas <- anyo2015[anyo2015$Coordenada.UTM..Y. > 0, ]

#plot(anyo2015$Coordenada.UTM..X.,anyo2015$Coordenada.UTM..Y.)

#cities.shape<-readOGR("C:/R/PRAC2TIP/masa.shp")
#plot(cities.shape)
#plot(anyo2015$Coordenada.UTM..X.,anyo2015$Coordenada.UTM..Y.,add=TRUE)