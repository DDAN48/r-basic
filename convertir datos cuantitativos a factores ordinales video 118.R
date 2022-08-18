

crabs=read.table('./data/datacrab.txt',header = TRUE)

crabs=crabs[,-1]#omitimos primera columna
str(crabs)

#convertir la variable width en una variable ordinal por rango
min(crabs$width)

intervalos=cut(crabs$width,breaks = c(21,25,29,33,Inf), right=FALSE,labels = c('21-25','25-29','29-33','33-..'))

crabs['Intervalos']=ordered(intervalos)

tabla=table(crabs[,c(1,6)])
tabla

#frecuencia relativa
fr=prop.table(tabla,margin=1)
fr

#frecuencia relatica absoluta
fac=t(apply(fr,MARGIN =1,FUN=cumsum))
fac

#GRAFICANDO Frecuencia relativa

colores=c('cyan','cyan4','cyan1','cyan2')
barplot(t(fr),beside=TRUE,legend=TRUE,main='Diagramas de barras de frecuencias Relativas',col=colores,args.legend = list(x='topright',cex=1))


#generando grupos por tamaño

cw=crabs$width
#seleccíon de K grupos

#regla de raiz cuadrada
n=length(cw)
krc=sqrt(n)

krc=ceiling(krc)#ceiling redondea hacia arriba#13


#regla de Sturges

ksturges=ceiling(1+log(n))#7


#regla de Scott, usa amplitud para su calculo

Ascott=3.5*sd(cw)*n^(-1/3)#amplitud teorica
kscott=ceiling(diff(range(cw))/Ascott)#10

#regla de Freedman-Diaconis

Afd=2*(quantile(cw,0.75,names=FALSE)-quantile(cw,0.25,names=FALSE))*n^(-1/3)
kFD=ceiling(diff(range(cw))/Afd)#13
#-------------------------------------------------------------------------------------------------
#si trabajamos con la regla de Scott ya sabemos que la cantidad de intervalos son 10 
#y para la amplitud de esos intervalos se calcula de la siguiente manera :

A=diff(range(cw))/kscott#1.25 , LO LLEVAMOS A 1.3 por que estamos trabajando con un decimal
A=1.3
#ahora hay que calcular el valor de los rangos empezando por el pirmer intervalo

precision=0.1#elegida a criterio
p=precision
L1=min(cw)-1/2*p


#el total de rango se calcula de manera recursiva l2=l1+a, l3=l2+a

L=L1+A*(0:10)

#Ahora calculamos los medios o marcas de clases de los valores

X1=(L1[1]+L[2])/2

X=X1+A*(0:9)
X

#agregando los intervalos 

intervalos=cut(crabs$width,breaks = L,right = FALSE)

#tabla de frecuecia absoluta
muestra= sample(1:6,25 ,replace=T)
tmfreAb=table(muestra)
tmfreAb

#tabla de frecuencia relativa
tmfRe=prop.table(tmfreAb)
tmfRe


#tabla de frecuencia relativa acumulada
tmfReAcum=cumsum(tmfRe)
tmfReAcum


#tabla de frecuecia absoluta acumulada
tmfreAbAcum=cumsum(tmfreAb)
tmfreAbAcum

#construyendo un dataframe de frecuencia 

df=data.frame(Puntuacion=1:6, tmfreAb=as.vector(tmfreAb) ,tmfRe=as.vector(tmfRe) ,tmfReAcum=as.vector(tmfReAcum) ,tmfreAbAcum=as.vector(tmfreAbAcum) )
df

#moda
which.max(df$tmfreAb)
vectorM=df[which.max(df$tmfreAb),1]
vectorM



#rango entre datos 
rango=diff(range(muestra))
rango

#cunatiles 
quantile(muestra,0.25)
quantile(muestra,0.75)

#Rango intercuartiles

IQR(muestra)

#varainza muestral

var(muestra)
#desviacion estandar mustreal

sd(muestra)

#varianza

var(muestra)*( length(muestra)-1 )/length(muestra)

#desviacion standar

sd(muestra)*sqrt(( length(muestra)-1 )/length(muestra))

#resumen  estadisticos 

summary(muestra)

summary(df)

summary(subset(crabs,color==5))

#la funcion by es un groupby de python , agrupa en funcion de una variable y ejecuta la funcion
by(iris[,c(1,3)],iris$Species,FUN = summary)

#la funcion aggregate hace lo mismo que by pero ordenado en otro formato


aggregate(cbind(iris$Sepal.Length,iris$Petal.Length)~Species,data=iris,FUN = summary)



boxplot(iris[,1:4],col='lightblue',fill=c("orange"))
boxplot(iris$Petal.Length~Species,data=iris,main="Species por longitud Petalo",ylab='Longitud Petalo',col='lightblue')



v=crabs$weight


medias=aggregate(v~color,data=crabs,FUN = mean)
medias
boxplot(crabs$weight~color,data = crabs,notch=TRUE)
points(medias,col='green',pch=15)
str(boxplot(crabs$weight~color,data = crabs,notch=TRUE))

hist(crabs$width,right = F) 
hist(crabs$width,breaks=5, right= F,plot = T) 
histRel(crabs$width,5)
