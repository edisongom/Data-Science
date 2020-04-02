#Verificar si existe outliers con un 99% de confianza
library(mlbench)
data(Ozone)
names(Ozone)
boxplot(Ozone$V13,col=(c("gold")),main="V13",range =3, horizontal=TRUE, xlab="Cantidad")

p=quantile(Ozone$V13,prob =c(0.99) )
abline(v = p, col="red", lwd=3)
p
abline(v = p, col="red",, lwd=2)
60*9.21

x <- Ozone$V13
h<-hist(x, breaks=20, col="#3399FF",ylab="Frecuencia", 
        main="Histograma Deuda") 
legend("topright", c( "Percentil 99"), fill=c("red"),cex=1,box.lty=0)

xfit<-seq(min(x),max(x),length=40) 
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x)) 
yfit <- yfit*diff(h$mids[1:2])*length(x) 
lines(xfit, yfit, col="#000033", lwd=2)
####PERCE

p=quantile(x,prob =c(0.99) )
abline(v = p, col="red", lwd=2)


#################HISTOGRAMA SIN OUTLIER
OZOUT<-Ozone[!(Ozone$V13>=400),]

x <- OZOUT$V13
h<-hist(x, breaks=20, col="#3399FF",ylab="Frecuencia", 
        main="Histograma V13") 
legend("topright", c( "Percentil 99"), fill=c("red"),cex=1,box.lty=0)

xfit<-seq(min(x),max(x),length=40) 
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x)) 
yfit <- yfit*diff(h$mids[1:2])*length(x) 
lines(xfit, yfit, col="#000033", lwd=2)
####PERCE

p=quantile(x,prob =c(0.99) )
abline(v = p, col="red", lwd=2)



###################Libreria dlookr

### informe de outliers con un 95% de confianza,

install.packages("dlookr")
library(dlookr)
dfOzone=as.data.frame.matrix(Ozone) 

plot_outlier(dfOzone)
diagnose_outlier(Ozone)


