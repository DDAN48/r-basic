install.packages('tidyverse',dep=TRUE)
install.packages('gmodels',dep=T)
x=sample(1:5,size =15,replace = T)
x
y=factor(sample(c('si','no'),size =37,replace = T))
ty=table(y)

table(x)

z=factor(x,levels = 1:7)

table(y)['no']
prop.table(table(y))

names(which(ty=max(ty)))

m=rep('male',14)
f=rep('female',23)

vmf=c(m,f)

vmf

tmf=table(vmf)

prop.table(tmf)

names(which(tmf==max(tmf)))


tvmfy=table(vmf,y)

t(tvmfy)

prop.table(tvmfy,margin=1)
prop.table(tvmfy,margin=2)

CrossTable(vmf,y,prop.chisq = FALSE)
#tabla ordinal 
notas=ordered(c('s','a','e','s','s','a','a','e','e','s','a','e','s','a','e','b'),levels=c('a','e','s','b'))
table(notas)

#frecuencias absolutas de datos ordinales


set.seed(2018)
clientes=sample(1:5,size=50,replace=T)


clientes=ordered(clientes, levels=c(1,2,3,4,5))

clientes=table(clientes)
c2=prop.table(clientes)
barplot(cumsum(c2),main='Diagrama de Frecuencia')


y=factor(sample(c('Muy.corto','normal','corto','largo','Muy.largo'),size =100,replace = T))

tablao=ordered(y,levels=(c('Muy.corto','corto','normal','largo','Muy.largo'))
tabla=table(tablao)
tablaRelativa=prop.table(tabla)
tablaAcumuladaR=cumsum(tablaRelativa)
tablaAcumuladaA=cumsum(tabla)
barplot(tablaAcumuladaA)
barplot(tablaAcumuladaR)

#creando una tabla con las zonas donde estan las jirafas

zonas=rep(c('A','B','C','D'),c(35,20,35,10))

tablajirafa=data.frame(zonas,tablao)

TJA=apply(prop.table(table(tablajirafa),margin =1), MARGIN =1,FUN=cumsum)
prop.table(TJA,margin =1)

barplot((prop.table(TJA,margin =1)),beside=TRUE,legend=TRUE, args.legend =list(x='topleft',cex=0.55))

