#install.packages("dplyr")
#install.packages('ggplot2')
#install.packages('plotrix')
#install.packages('psych')
#install.packages("ggpubr")
library(dplyr)
library(arules)
library(arulesViz)
library(ggplot2)
library(car)
library(RcmdrMisc)
library(rgdal)
library(plotrix)
library(psych)
library(nortest)
library(ggpubr)


#########################################################1 Carga y exploracion del Dataset

#Indicamos el directorio de los ficherros
setwd('C:/R/PRAC2TIP')

#leemos los ficheros
anyo2015<-read.csv('2015_accidents.csv',sep=",",header=TRUE,dec = ",")

#Variables
names(anyo2015)

#sumario del dataset
summary(anyo2015)

#Valoers unicos numero de expdiente
unique(anyo2015$Número.d.expedient)

#Variable codigo distrito
table(anyo2015$Codi.districte)

#Variable nombre distrito
table(anyo2015$Nom.districte)

#Variable codigo barrio
table(anyo2015$Codi.barri)

#Variable nombre barrio
table(anyo2015$Nom.barri)

#Variable codigo calle
table(anyo2015$Codi.carrer)

#varariable numero postal
table(anyo2015$Num.postal.caption)

#Descripcion dia de la semana
table(anyo2015$Descripció.dia.setmana)

#Dia de la semana
table(anyo2015$Dia.setmana)

#Descripcion tipo de dia
table(anyo2015$Descripció.tipus.dia)

#Descripcion NK.anyo
table(anyo2015$NK.Any)

#Mes.de.any
table(anyo2015$Mes.de.any)

#Nom.mes
table(anyo2015$Nom.mes)

#Valores dia mes
range(anyo2015$Dia.de.mes,na.rm=TRUE)
table(anyo2015$Dia.de.mes)

#hora del dia
range(anyo2015$Hora.de.dia,na.rm=TRUE)
table(anyo2015$Hora.de.dia)

#Descipcion causa vianant
table(anyo2015$Descripció.causa.vianant)

#Tipo vehiculo implicado
table(anyo2015$Desc..Tipus.vehicle.implicat)

#Descripcion sexo
table(anyo2015$Descripció.sexe)

#Descipcion de los tipos de personas
table(anyo2015$Descripció.tipus.persona)

#Valores edad
table(anyo2015$Edat)

#eliminamos los desconocidos
a2015OK <- subset(anyo2015,anyo2015$Edat != 'Desconegut')
#Pasamos de factor a numerico
a2015OK$Edat<-as.numeric(levels(a2015OK$Edat))[a2015OK$Edat]
#Comparamos la tabla con la anterior
table(a2015OK$Edat)
#sumario del numerico
summary(a2015OK$Edat)
#Desviacion estandar
sd(a2015OK$Edat)

#Descipcion victimas
table(anyo2015$Descripció.victimització)

#coordenadas UTM XY
range(anyo2015$Coordenada.UTM..X.)
range(anyo2015$Coordenada.UTM..Y.)



#########################################################2 seleccion de variables
#eliminamos columnas
a2015OK<-select(a2015OK,-1,-3,-5,-7,-10,-11,-13)
#variables del dataframe
str (a2015OK)
#summary(a2015OK)
#Eliminacion de los valores -1
a2015OK<-a2015OK[a2015OK$Codi.districte != -1, ]
a2015OK<-a2015OK[a2015OK$Codi.barri != -1, ]
a2015OK<-a2015OK[a2015OK$Codi.carrer != -1, ]
a2015OK<-a2015OK[a2015OK$Descripció.sexe != 'Desconegut', ]


#########################################################3 Limpieza de datos Identificación y tratamiento de valores extremos

#Comprobamos los valores nulos
summarise_all(a2015OK, funs(sum(is.na(.))))
#dataset limpio
str (a2015OK)
#busqueda outliers edad
boxplot.stats(a2015OK$Edat)
#Grafico boxplot
boxplot(a2015OK$Edat)

##########################################################4.1 Análisis temporal de los accidentes.


#Heridos en cada mes
table(a2015OK$Descripció.victimització,a2015OK$Nom.mes)
prop.table(table(a2015OK$Descripció.victimització,a2015OK$Nom.mes),1)
barplot(with(a2015OK,table(Descripció.victimització,Nom.mes)),
        xlab="Leves:Verde, Graves:Rojo, Muerto:Negro", ylab="Frecuencia",
        col=c("red","green","black"), main="Heridos por meses")


# Heridos dia del mes
table(a2015OK$Descripció.victimització,a2015OK$Dia.de.mes)
prop.table(table(a2015OK$Descripció.victimització,a2015OK$Dia.de.mes),1)
diames<-c(table(unlist((a2015OK$Dia.de.mes))))
barplot(with(a2015OK,table(Descripció.victimització,Dia.de.mes)),
        xlab="Leves:Verde, Graves:Rojo, Muerto:Negro", ylab="Frecuencia",
        col=c("red","green","black"), main="Heridos por dias mes")

#Heridos en cada dia de la semana
table(a2015OK$Descripció.victimització,a2015OK$Descripció.dia.setmana)
prop.table(table(a2015OK$Descripció.victimització,a2015OK$Descripció.dia.setmana),1)
dias<-c(table(unlist(a2015OK$Descripció.dia.setmana)))
barplot(with(a2015OK,table(Descripció.victimització,Descripció.dia.setmana)),
        xlab="Leves:Verde, Graves:Rojo, Muerto:Negro", ylab="Frecuencia",
        col=c("red","green","black"), main="Heridos por dias semana")



########################################################### 4.2 Analisis de social personas heridas
#Eliminamos factor desconegur
a2015OK$Descripció.sexe<-factor(a2015OK$Descripció.sexe)

#Edad Maxima
max(a2015OK$Edat)

#edad minima
min(a2015OK$Edat)

#Histograma edad
hist(a2015OK$Edat,main='Edad de los heridos',col='15',ylab='Frequencia',xlab='Edad')

#Frequencias edad y sexo
edadsexo= data.frame(a2015OK$Edat,a2015OK$Descripció.sexe)
summary(edadsexo)

#Normalidad edad
if ((ad.test(a2015OK$Edat)$p.value)>0.05)
{ print("Distribucion normal")
}else
{print("No es una distribucion normal")}


#Homogeneidad de las varianzas
fligner.test(a2015OK$Edat~a2015OK$Descripció.sexe, data=a2015OK)
if ((fligner.test(a2015OK$Edat~a2015OK$Descripció.sexe, data=a2015OK))>0.05)
{ print("Muestras homogeneas")
}else
{print("Muestras no homogeneas")}

#Desviacion por edad y sexo
aggregate(a2015OK$Edat,by=list(a2015OK$Descripció.sexe),mean,na.rm=TRUE)
boxplot(a2015OK$Edat~a2015OK$Descripció.sexe)


#Relacion heridas y edad
table(a2015OK$Descripció.victimització,a2015OK$Edat)
#Relacion heridas y sexo
table(a2015OK$Descripció.victimització,a2015OK$Descripció.sexe)

#segun heridas
valores<-c(table(unlist((a2015OK$Descripció.victimització))))
etiqueta<-c("Graves","Leves","Muertos")
etiqueta<-paste(etiqueta,valores)
pie3D(table(anyo2015$Descripció.victimització))
pie(valores,etiqueta, col=rainbow(3),main='Tipos y cantidad de heridos')

#heridas-sexo
group_by(a2015OK,Descripció.victimització) %>%
  summarise(
    count = n(),
    mean = mean(Edat, na.rm = TRUE),
    sd = sd(Edat, na.rm = TRUE),
    median = median(Edat, na.rm = TRUE),
    IQR = IQR(Edat, na.rm = TRUE)
  )

#Boxplot heridas-sexo
ggboxplot(a2015OK, x = "Descripció.victimització", y = "Edat", 
          color = "Descripció.victimització", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
          order = c("Ferit lleu", "Ferit greu", "Mort"),
          ylab = "Edad", xlab = "Heridas")


#Grafico barras heridos-sexo
barplot(with(a2015OK,table(Descripció.sexe,Descripció.victimització)),beside=TRUE,legend=TRUE,
        xlab="Tipo de herido", ylab="Frecuencia",
        col=c("lightblue","pink"), main="Gravedad herridas según sexo")



#heridas-hombres
Hombres<-a2015OK[a2015OK$Descripció.sexe == 'Home', ]
group_by(Hombres,Descripció.victimització) %>%
  summarise(
    count = n(),
    mean = mean(Edat, na.rm = TRUE),
    sd = sd(Edat, na.rm = TRUE),
    median = median(Edat, na.rm = TRUE),
    IQR = IQR(Edat, na.rm = TRUE)
  )

#Boxplot heridas-hombres
ggboxplot(Hombres, x = "Descripció.victimització", y = "Edat", 
          color = "Descripció.victimització", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
          order = c("Ferit lleu", "Ferit greu", "Mort"),
          ylab = "Edad", xlab = "Heridas Hombres")

#heridas-Mujeres
Mujeres<-a2015OK[a2015OK$Descripció.sexe == 'Dona', ]
group_by(Mujeres,Descripció.victimització) %>%
  summarise(
    count = n(),
    mean = mean(Edat, na.rm = TRUE),
    sd = sd(Edat, na.rm = TRUE),
    median = median(Edat, na.rm = TRUE),
    IQR = IQR(Edat, na.rm = TRUE)
  )

#Boxplot heridas-mujer
ggboxplot(Mujeres, x = "Descripció.victimització", y = "Edat", 
          color = "Descripció.victimització", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
          order = c("Ferit lleu", "Ferit greu", "Mort"),
          ylab = "Edad", xlab = "Heridas Mujeres")



#Heridos segun tipo vehiculo y edad
#test Kruskal-Wallis
group_by(a2015OK,Desc..Tipus.vehicle.implicat) %>%
  summarise(
    count = n(),
    mean = mean(Edat, na.rm = TRUE),
    sd = sd(Edat, na.rm = TRUE),
    median = median(Edat, na.rm = TRUE),
    IQR = IQR(Edat, na.rm = TRUE)
  )


#boxplot tipo vehiculo-edad
boxplot(Edat~Desc..Tipus.vehicle.implicat,data=a2015OK, main="Relacion tipo vehiculo-Edad", 
        xlab="Tipo de vehiculo", ylab="Edad")

#Grafico barras tipo herdias-vehiculo
barplot(with(a2015OK,table(Descripció.victimització,Desc..Tipus.vehicle.implicat)),beside=TRUE,legend=TRUE,
        xlab="Tipo Heridas", ylab="Frecuencia",
        col=c("pink","lightblue","green"), main="Heridos segun vehiculos")

#Grafico barras tipo persona-sexo
barplot(with(a2015OK,table(Descripció.tipus.persona,Descripció.sexe)),beside=TRUE,legend=TRUE,
        xlab="Sexo", ylab="Frecuencia",
        col=c("pink","lightblue","green"), main="Tipo de persona implicada-Sexo")


###########################################################4.3 Analisis fallecidos

Muertos<-a2015OK[a2015OK$Descripció.victimització == 'Mort', ]
#eliminamos facor desconegut
Muertos$Descripció.sexe<-factor(Muertos$Descripció.sexe)

group_by(Muertos,Descripció.sexe) %>%
  summarise(
    count = n(),
    mean = mean(Edat, na.rm = TRUE),
    sd = sd(Edat, na.rm = TRUE),
    median = median(Edat, na.rm = TRUE),
    IQR = IQR(Edat, na.rm = TRUE)
  )

#total por sexo
plot(Muertos$Descripció.sexe,xlab="Sexo", ylab="Frecuencia",col=c("pink","lightblue"), main="Fallecidos por sexo")

#Muertos barrios
barplot(with(Muertos,table(Descripció.sexe,Codi.districte)),legend=TRUE,
        xlab="Codigos de distrito", ylab="Frecuencia",
        col=c("red","blue"), main="Fallecidos por distrito")


#Muertos vehiculos
barplot(with(Muertos,table(Descripció.sexe,Desc..Tipus.vehicle.implicat)),legend=TRUE,
        xlab="Codigos de distrito", ylab="Frecuencia",
        col=c("pink","lightblue"), main="Fallecidos por distrito")

#Muertos tipo persona
barplot(with(Muertos,table(Descripció.sexe,Descripció.tipus.persona)),legend=TRUE,
        xlab="Codigos de distrito", ylab="Frecuencia",
        col=c("red","blue"), main="Fallecidos por distrito")

#muertos dia semana
barplot(with(Muertos,table(Descripció.sexe,Descripció.dia.setmana)),legend=TRUE,
        xlab="Leves:Verde, Graves:Rojo, Muerto:Negro", ylab="Frecuencia",
        col=c("pink","lightblue"), main="Heridos por dias semana")



###########################################################4.4 Analisis geoespacial
#Eliminamos corrdenadas -1
anyo2015$Coordenada.UTM..X.<-as.numeric(anyo2015$Coordenada.UTM..X.)

anyo2015$Coordenada.UTM..Y.<-as.numeric(anyo2015$Coordenada.UTM..Y.)
coor<-anyo2015[anyo2015$Coordenada.UTM..X. != -1, ]
coor<-subset(coor,select = -c(1,2,4,6,7,10,11))
#creamos csv para analisis en Qgis
write.csv(coor, file="2015_coordenadas.csv")

#Datoscreados
write.csv(Hombres, file="hombres.csv")
write.csv(Mujeres, file="mujeres.csv")
write.csv(Muertos, file="fallecidos.csv")
write.csv(a2015OK, file="a2015OK.csv")

#Alumno Jose Luis Fernandez Losada
#jfernandezlosada

