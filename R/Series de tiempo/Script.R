library(tseries)
#install.packages("trend")
library(trend)
#install.packages("kendall")
library(Kendall)
#install.packages("mice")
library(mice)
#install.packages("Kendall")
library(Kendall)
library(forecast)
# Instalar el paquete 'readr' el paquete más práctico para leer ficheros planos&amp;amp;lt;/span&amp;amp;gt;
install.packages("readr")
# Cargar el paquete

BASE=gasolinaa


attach(BASE)
names(BASE)
summary(BASE)
tas=Galones
summary(tas)
ts.plot(tas)
x=tas
##################TRANSFORMACION A SERIE DE TIEMPO#####################
y = ts(x,frequency=12,start=c(1960,1))
##########GRAFICO DEL COMPORTAMIENTO DE LA SERIE; TIENE TENDENCIA Y ESTACIONALIDAD##########
ts.plot(y, col="Blue", main="Gráfico Serie Temporal")
###############DESCOMPOSICION DE LA SERIE###############
plot(decompose(y), col="Blue")

#SERIE TEMPORAL#
####### ver si la serie es estacionaria#########
#####Test de Phillips-Perron######

library(tseries)
PP.test(y)
######Test de Dickey-Fuller#######
adf.test(y,k=12)
###########TEST DE TENDENCIA###############
########Test de Cox-Stuart#######
install.packages("trend")
library(trend)
cs.test(y)
#######MANN-KENDALL###########
####Sin considerar estacionalidad######
install.packages("kendall")
library(Kendall)
MK = MannKendall(y)
summary(MK)
#######Considerando estacionalidad########
MKS= SeasonalMannKendall(y)
summary(MKS)
######### Test de ESTACIONALIDAD#########
######## Test de Kruskal-Wallis#######

install.packages("BSDA")
library(BSDA)
attach(BASE)
mm=lm(tas~Mes)
length(Meses)
length(tas)
kruskal.test(tas~Meses)


#BREUSCH PAGAN TEST# 
#H0: los errores son homocedásticos.#
#H1: los errores son heterocedásticos.#
install.packages("lmtest")
library(lmtest)
mod=lm(tas~Meses)
kruskal.test(mod)
bptest(mod)
#ACF Y PACF PARA CADA PROCESO DE DIFERENCIACION#
#SERIE ORIGINAL#
library(tseries)
ts.plot(y,col="Blue", main="Gráfico Serie Temporal",gpars=list(xlab="Año", ylab="Temperatura media"))
acf(y,lag=100)
pacf(y,lag=100)	
#DICKEY FULLER H0: LA SERIE NO ES ESTACIONARIA vs H1: LA SERIE ES ESTACIONARIA#
adf.test(y)

# Eliminar Tendencia

seriebase<-diff(y)
plot(seriebase)
acf(seriebase,lag.max=50,main="Serie sin tendencia")
pacf(seriebase,lag.max=50,main="Serie sin tendencia")
cs.test(difff)

kruskal.test(difff~ta)
length(T)
ta=1:length(seriebase)
### Eliminar Estacionalidad

dif2<-diff(seriebase,lag=12)
cs.test(dif2)

kruskal.test(dif2~taa)
taa=1:length(dif2)
acf(dif2,lag.max=60,main="Serie sin estacionalidad")
pacf(dif2,lag.max=60,main="Serie sin estacionalidad")
adf.test(dif2)
plot.ts(dif2)
t.test(dif2)#COMPARACION DE MEDIAS
ks.test(dif2,pnorm,mean(dif2),sd(dif2))
qqnorm(dif2)
qqline(dif2,COL=50,LWD=2)
H<-hist(dif2, col = "LIGHT BLUE",YLAB ="FRECUENCIA",MAIN = "NORMALIDAD SEGUNDA DIFERENCIACIÓN" )
XFIT <- seq(min(dif2), max(dif2), length = 60)
YFIT <- dnorm(XFIT, mean = mean(dif2), sd = dif2)
YFIT <- YFIT * diff(H$MIDS[1:2]) * length(dif2)
lines(XFIT, YFIT, COL = "ORANGE", LWD = 2)
bptest()


install.packages("forecast")
library(forecast)
#################HEGY
library(uroot)
hegy.test(y,deterministic=c(1,1,1),lag.method ="AIC",pvalue = "RS")

hegy.test(difff,deterministic=c(1,0,1),lag.method ="AIC",pvalue = "RS")
kruskal.test(difff~TT)
length(difff)
TT=1:length(difff)
hegy.test(difff2,deterministic=c(1,0,0),lag.method ="AIC",pvalue = "RS")
#BOX JENKINS# 
#Posibles Modelos para los datos#

var(y)
var(difff)
var(dif2)
difff=diff(y)

difff2=diff(difff,lag=12)
acf(difff2,lag=50)



acf(difff,lag=50)
pacf(difff,lag=50)


###########analisis serie diferenciada###############################
library(stats)
t.test(difff)
ks.test(difff,pnorm,mean(difff),sd(difff))



qqnorm(difff)
qqline(difff,COL=5,LWD=2)
H<-hist(difff, COL = "LIGHT BLUE",YLAB ="FRECUENCIA",MAIN = "NORMALIDAD SEGUNDA DIFERENCIACIÓN" )
XFIT <- seq(min(difff), max(difff), length = 40)
YFIT <- dnorm(XFIT, mean = mean(difff), sd = difff)
YFIT <- YFIT * DIFF(H$MIDS[1:2]) * length(difff)
lines(XFIT, YFIT, COL = "ORANGE", LWD = 2)
ts.plot(difff, col="SKYBLUE", LWD=2, main="ANÁLISIS HOMOCEDASTICIDAD")#HOMOCEDASTICIDAD
dif2

######BARLET
length(dif2)
A<-c(rep(1:12, 14),1,2,3,4,5,6,7,8,9,10,11)
F<-(1:9)
G<-c(A,F)

MES<-as.factor(G)
MES=MES[-1:-13]
bartlett.test(dif2~A)
BARTLETT.TEST(RESPOLREGION~MES)
library(lmtest)
bptest(dif2~A)



TTT=1:length(dif2)


attach(BASE)
library(lmtest)
bptest(tas~Meses)
attach(dif2)
names(dif2)




mod1=arima(y,order=c(2,1,1),seasonal=list(order=c(0,1,4),period=12))
mod1
coeftest(mod1)
mod3=auto.arima(y)
mod3
coeftest(mod3)
mod2=arima(y,order=c(2,1,0),seasonal=list(order=c(0,1,1),period=12))
ks.test(residuos2)
mod2
coeftest(mod2)
#Diagnostico del modelo#
tsdiag(mod1)
tsdiag(mod2)
tsdiag(mod3)


residuos1=resid(mod1)
residuos2=resid(mod2)
residuos3=resid(mod3)


Box.test(residuos1, lag = 10, type = c("Box-Pierce"))
Box.test(residuos1, lag = 10, type = c("Ljung-Box"))
Box.test(residuos1, lag = 190, type = c("Box-Pierce"))
Box.test(residuos1, lag = 190, type = c("Ljung-Box"))

Box.test(residuos2, lag = 10, type = c("Box-Pierce"))
Box.test(residuos2, lag = 10, type = c("Ljung-Box"))
Box.test(residuos2, lag = 190, type = c("Box-Pierce"))
Box.test(residuos2, lag = 190, type = c("Ljung-Box"))

Box.test(residuos3, lag = 10, type = c("Box-Pierce"))
Box.test(residuos3, lag = 10, type = c("Ljung-Box"))
Box.test(residuos3, lag = 190, type = c("Box-Pierce"))
Box.test(residuos3, lag = 190, type = c("Ljung-Box"))



mod1$aic
mod2$aic
mod3$aic



t.test(residuos4)
ks.test(residuos4,pnorm,mean(residuos4),sd(residuos4))

t.test(dif2)
ks.test(dif2,pnorm,mean(dif2),sd(dif2))
library(lmtest)
bptest(Ingresos~y)


#


predict(mod1, n.ahead = 19)
predict(mod2, n.ahead = 19)
predict(mod3, n.ahead = 19)
predict(mod4, n.ahead = 19)


LH.pred=predict(mod4,n.ahead=19)

plot.ts(Ingresos,lwd=2,col="black",xlim=c(0,144),ylim=c(2000,12000),main="Predicciones")
points(125:144,c(y[125],LH.pred$pred[1:19]),lwd=2,col="purple",type="l")
lines(125:143,c(LH.pred$pred)+2*c(LH.pred$se),col="orange",lty=3)
lines(125:143,c(LH.pred$pred)-2*c(LH.pred$se),col="orange",lty=3)

####################REGresION POLINOMIAL 1# ####################
ts.plot(y)
t=1:length(y)
tt=t*t
ttt=t*t*t
Modelo=lm(y~t+tt+ttt)
summary(Modelo)
ts.plot(residuals(Modelo),ylab="residuos del Modelo")
plot(t,y,type='l')
lines(t,fitted(Modelo),col='red')
sum(residuals(Modelo)^2)
z=residuals(Modelo)
mserp=mean(z^2)
AIC(Modelo)

#################REGresION DUMMY######################## 

Mes = as.factor(c(rep(1:12, 16)))
length(Mes)
fit = lm(tas~Mes)
summary(fit)
AIC(lm(tas~Mes))
ts.plot(tas)
lines(fit$fit,lwd=1,col="red")
res2<-fit$res
ts.plot(res2)
msedum=mean(res2^2)
AIC(fit)

##ARMONICO
T<-1:length(y)
T2=T**2
W<-1/12 #FRECUENCIA
MODELO222<-lm(y~ sin(2*pi*W*T)+cos(2*pi*W*T))
summary(MODELO222)
AIC(MODELO222)


##CONJUNTA POLINIOMIAL MAS DUMMY
FIT = lm(y ~ T+T2+Mes)
summary(FIT)
plot(T,y,TYPE="L", main="AJUSTE REGRESIÓN POLINOMIAL + REGRESIÓN DUMMY ")
lines(T,fitted(FIT),col="VIOLET RED",LWD=2)
AIC(FIT)
residuodu=resid(FIT)
Box.test(residuodu, lag = 10, type = c("Box-Pierce"))
Box.test(residuos3, lag = 190, type = c("Ljung-Box"))

##CONJUNTA POLINIOMIAL MAS ARMONICO
T<-1:length(y)
T2=T**2
W<-1/12 #FRECUENCIA
MODELO<-lm(y~ T+T2+sin(2*pi*W*T)+cos(2*pi*W*T))
summary(MODELO)
AIC(MODELO)
#NORMALIDAD
ks.test(mod2$res,pnorm,mean(mod2$res),sd(mod2$res))
HH=hist(mod2$res,col="light blue",ylab="frecuencia",main="Normalidad Residuos SARIMA")
XFIT <- seq(min(mod2$res), max(mod2$res), length = 40)
YFIT <- dnorm(XFIT, mean = mean(mod2$res), sd = sd(mod2$res))
YFIT <- YFIT * diff(HH$mids[1:2]) * length(mod2$res)
lines(XFIT, YFIT, col = "VIOLET RED", LWD = 2)

#HOMOCEDASTICIDAD
A<-rep(seq(1:12),16)
F<-(0:12)
G<-c(A,F)
length(G)
MES<-as.factor(G)
RESIDUOSARIMA11=resid(mod2)
MES=MES[-1:-13]
bptest(RESIDUOSARIMA11~MES)
length(RESIDUOSARIMA11)
length(MES)
length(mod2)
#NO CORRELACIÓN
tsdiag(mod2)

#NORMALIDAD POLI+DUMMY
ks.test(FIT$res,pnorm,mean(FIT$res),sd(FIT$res))
HH=hist(FIT$res,col="light blue",ylab="frecuencia",main="Normalidad Residuos POLINOMIAL+DUMMY")
XFIT <- seq(min(FIT$res), max(FIT$res), length = 40)
YFIT <- dnorm(XFIT, mean = mean(FIT$res), sd = sd(FIT$res))
YFIT <- YFIT * diff(HH$mids[1:2]) * length(FIT$res)
lines(XFIT, YFIT, col = "VIOLET RED", LWD = 2)
qqnorm(FIT$res)
qqline(FIT$res, col="VIOLET RED",LWD=2)


#HOMOCEDASTICIDAD POLI+DUMMY
RESIDUOSDUMMY=resid(FIT)
bptest(RESIDUOSDUMMY~MES)

#NO CORRELACIÓN POLI+DUMMY
Box.test(FIT$res, lag = 36, type = c("Ljung-Box"))


#NORMALIDAD POLI+ARMO
ks.test(MODELO$res,pnorm,mean(MODELO$res),sd(MODELO$res))
HH=hist(MODELO$res,col="light blue",ylab="frecuencia",main="Normalidad Residuos POLINOMIAL+DUMMY")
XFIT <- seq(min(MODELO$res), max(MODELO$res), length = 40)
YFIT <- dnorm(XFIT, mean = mean(MODELO$res), sd = sd(MODELO$res))
YFIT <- YFIT * diff(HH$mids[1:2]) * length(MODELO$res)
lines(XFIT, YFIT, col = "VIOLET RED", LWD = 2)
qqnorm(MODELO$res)
qqline(MODELO$res, col="VIOLET RED",LWD=2)


#HOMOCEDASTICIDAD POLI+DUMMY
RESIDUOSDUMMY=resid(MODELO)
bptest(RESIDUOSDUMMY~MES)

#NO CORRELACIÓN POLI+DUMMY
Box.test(FIT$res, lag = 36, type = c("Ljung-Box"))




















#PREDICCIONES POLINOMIAL+DUMMY (30)#

FIN<-lm(tas ~ T+T2+Mes)
summary(FIN)
mean(residuals(FIN)**2)
NEW<-cbind(193:222,(193:222)**2,as.factor(c(rep(1:12, 2),1,2,3,4,5,6)))
NEW2<-data.frame(T=NEW[,1],T2=NEW[,2],Mes=as.factor(NEW[,3]))
PR1<-predict(FIN,NEW2,interval=c("prediction"))
plot(1:2222,seq(1,2300,length=2222),type='n', main = "PREDICCION A 2 AÑOS MODELO POLINOMIAL + DUMMY")
lines(T,tas,col=1)
lines(193:222,PR1[,1],col='BLUE',lwd=1)
lines(193:222,PR1[,2],col='RED',lwd=1)
lines(193:222,PR1[,3],col='RED',lwd=1)
ts.plot(FIN$res, col="BLUE")








seriesin=BASE[1:173,]
attach(seriesin)
Galones
Mess = as.factor(c(rep(1:12, 14),1,2,3,4,5))

yy = ts(Galones,frequency=12,start=c(1991,1))
fin<-lm(y~Mes)
summary(fin)
mean(residuals(fin)^2)
new=cbind(173:192,as.factor(c(rep(1:12, 1),1,2,3,4,5,6,7,8)))
new2=data.frame(t=new[,1],Mes=as.factor(new[,2]))
pr1=predict(fin,new2,interval="prediction")
plot(1:192,seq(2,15,length=192),type='n', main="Predicciones")
lines(t,y,col=1)
lines(173:192,pr1[,1],col='blue',lwd=2)
lines(173:192,pr1[,2],col='red',lwd=1)
lines(173:192,pr1[,3],col='red',lwd=1)
ts.plot(fin$res)
AIC(fin)

#TEST DE BLANCURA SARIMA
tsdiag(mod3)
#NORMALIDAD
ks.test(mod3$res,pnorm,mean(mod3$res),sd(mod3$res))
H<-hist(mod3$res, col = "LIGHT BLUE",ylab ="FRECUENCIA",main = "NORMALIDAD resIDUOS SARIMA" )
XFIT <- seq(min(mod3$res), max(mod3$res), length = 40)
YFIT <- dnorm(XFIT, mean = mean(mod3$res), sd = sd(mod3$res))
YFIT <- YFIT * diff(H$mids[1:2]) * length(mod3$res)
lines(XFIT, YFIT, col = "VIOLET RED", lwd = 2)

#HOMOCEDASTICIDAD

RESIDUOSARIMA11=resid(mod3)

bptest(RESIDUOSARIMA11~Mes)

#NO CORRELACIÓN
tsdiag(mod3)

#DUMMY
ks.test(FIT$res,pnorm,mean(FIT$res),sd(FIT$res))
qqnorm(FIT$res)
qqline(FIT$res, col="VIOLET RED",lwd=2)
#HOMOCEDASTICIDAD
RESIDUOSDUMMY=resid(FIT)
bptest(RESIDUOSDUMMY~Mes)

#NO CORRELACIÓN
box.test(FIT$res, LAG = 36, type = c("LJUNG-BOX"))

#ARMONICO
ks.test(MODELO$res,pnorm,mean(MODELO$res),sd(MODELO$res))
qqnorm(MODELO$res)
qqline(MODELO$res, col="VIOLET RED",lwd=2)
#HOMOCEDASTICIDAD
RESIDUOSARMONICO=resid(MODELO)
bptest(RESIDUOSARMONICO~Mes)

#NO CORRELACIÓN
box.test(MODELO$res, lag = 36, type = c("LJUNG-BOX"))

#H
#H
#PREDICCIONES#
#DUMMY
FIN<-lm(tas ~ T+T2+Mes)
summary(FIN)
mean(residuals(FIN)**2)
NEW<-cbind(94:121,(94:121)**2,as.factor(c(rep(1:12, 2),1,2,3,4)))
NEW2<-data.frame(T=NEW[,1],T2=NEW[,2],Mes=as.factor(NEW[,3]))
PR1<-predict(FIN,NEW2,interval="prediction")
plot(1:121,seq(55,150,length=121),type='n', main = "PREDICCION A 2 AÑOS MODELO POLINOMIAL + DUMMY")
lines(T,tas,col=1)
lines(94:121,PR1[,1],col='BLUE',lwd=1)
lines(94:121,PR1[,2],col='RED',lwd=1)
lines(94:121,PR1[,3],col='RED',lwd=1)
ts.plot(FIN, col="BLUE")

##PREDICCION SARIMA 
predict(mod3, n.ahead  = 24)

lh.pred<-predict(mod3,n.ahead=24)

plot.ts(tas,lwd=1,col="BLACK",XLIM=c(0,118),YLIM=c(55,150),main="PREDICCION PROXIMOS 2 AÑOS SARIMA (2,1,0)X(0,1,1)_12")
points(94:118,C(tas[94],lh.pred$pred),lwd=1,col="RED",type="L")
lines(lh.pred$pred+2*lh.pred$se,col="VIOLET RED",lty=1)
lines(lh.pred$pred-2*lh.pred$se,col="VIOLET RED",lty=1)


install.packages("Metrics")
library(Metrics)

###############MAPE POLINOMIAL DUMMY
FIT = lm(y ~ T+T2+Mes)
FITM=FIT[-173:-192,]

BASEEE=BASE[-173:-192,]
ba=BASE[173:192,]
FIN<-lm(tas ~ T+T2+Mes,BASEEE)
summary(FIN)
mean(residuals(FIN)**2)
NEW<-cbind(173:192,(173:192)**2,as.factor(c(rep(1:12, 1),1,2,3,4,5,6,7,8)))
NEW2<-data.frame(T=NEW[,1],T2=NEW[,2],Mes=as.factor(NEW[,3]))
PR1<-predict(FIN,NEW2,interval=c("prediction"))
predd=PR1[,1]
actual=ba[,2]
mape(actual,predd)

####################PREDICCION SARIMA


LH.PRED<-predict(mod3,n.ahead=24)



plot(y,lwd=1,col="blue",xlim=c(1960,1978),main="Predicción a 2 años Polinomial + Dummy ")
lines(LH.PRED$pred*1.002,col="red",lty=1)
lines(LH.PRED$pred+1.6*LH.PRED$se,col="violet red",lty=1)
lines(LH.PRED$pred-1.6*LH.PRED$se,col="violet red",lty=1)

############### MAPE SARIMA


attach(Gas)
names(Gas)
tass=Galones
ts.plot(tass)
X=tass
##################TRANSFORMACION A SERIE DE TIEMPO#####################
yy = ts(X,frequency=12,start=c(1960,1))

mod22=arima(yy,order=c(2,1,0),seasonal=list(order=c(0,1,1),period=12))

predsari<-predict(mod22,n.ahead=20)
predsari$pred
BASEEE=BASE[-173:-192,]
attach(BASEEE)
ba=BASE[173:192,]
FIN<-lm(tas ~ T+T2+Mes,BASEEE)
summary(FIN)
mean(residuals(FIN)**2)
NEW<-cbind(173:192,(173:192)**2,as.factor(c(rep(1:12, 1),1,2,3,4,5,6,7,8)))
NEW2<-data.frame(T=NEW[,1],T2=NEW[,2],Mes=as.factor(NEW[,3]))
PR1<-predict(FIN,NEW2,interval=c("prediction"))
predd=PR1[,1]
actual=ba[,2]
attach(PredSarima)
PredSarima=PredSarima[-1,]
actual=actual[-1]
mape(actual,PredSarima)





