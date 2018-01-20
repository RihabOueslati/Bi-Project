sem1<-read.csv(file.choose(),sep=";",header=T,na.string="")
sem2<-read.csv(file.choose(),sep=";",header=T,na.string="")
stif<-merge(sem1,sem2,all = TRUE)
stif
str(stif)
View(stif)
data<-aggregate(NB_VALD~JOUR,stif,FUN=sum,na.rm=TRUE,na.action=NULL)
end(st)
str(y)
start(st)
plot(st)
acf(st)
str(data)
View(data)
View(data)
summary (data)
library(zoo)
vect<-cbind(data$NB_VALD)
td<-seq(as.Date("2016/1/1"),as.Date("2016/12/31"),"days")
fonction_date<-zoo(vect,order.by=td)
length(fonction_date)
fct<-(fonction_date$Data)
plot(fct)
nb<-coredata(fonction_date)

str(fonction_date)
plot(fonction_date)
title("Nbre de validation par jour")
monthplot(fonction_date)
start(fonction_date)
end(fonction_date)
frequency(fonction_date)
plot(aggregate(fonction_date,by=as.yearmon, FUN=mean))
View(fonction_date)
acf(fonction_date)
boxplot(nb~cycle(nb))



library(ggplot2)

---------------------------

temps<-time(index(fonction_date))
model1<- lm(fonction_date ~ t1+t2)
res<-model1$residuals
acf(res)
summary(model1)
t_value=-3.494<-19.6
====> donc le coefficients temps est significatifs

temp_stand<- (t1-mean(t1))/sd(t1)
model2<-lm(fonction_date~ temps + temp_stand)
summary(model2)
summary(temp_stand)

SIN<-COS<- matrix(nr=length(temps),nc=183)
t<-c(0:(length(temps)-1))
for ( i in 1:183) {

 COS[,i]<-cos(2*pi*i*t/366)
 SIN[,i]<-sin(2*pi*i*t/366)}

model3<-lm(fonction ~temp_stand + SIN +COS )
summary(model3)
model4<- lm(fonction ~ temp_stand + SIN [,2]+COS[,2])
summary(model4)
acf(model3$residuals)
VD=function(k,n,t)
#k:nbr de mois
#n:nbr d'annees
#t:la freq
{x=rep(0,n*t)
for (i in 1:n){
x[k+(i-1)*t]<-1
}
x}
model5<- lm((log(fonction_date))~t1 +VD(1,1,1)+VD(2,1,1)+VD(3,1,1)+
VD(4,1,1)+VD(5,1,1)+VD(6,1,1)+VD(7,1,1)+VD(8,1,1)+VD(9,1,1)+
VD(10,1,1)+VD(11,1,1)+VD(12,1,1))
length(VD(1,1,366))
length(fonction_date)
--------------------

f1<-filter(log(fonction_date),rep(1/31,31))
plot(f1)
plot(f1)


f12<-na.approx(f1)
f12<-na.approx(f1)
acf(f12)
title("ACF Série Filtrée")
pacf(f12,50)
-----------------
seas<-cycle(fonction_date)
m3<-lm(fonction_date~t1+factor(seas))
adf.test(fonction_date$resid)
-----------------------
ARIMA 
acf(fonction_date,100)
fd<-diff(f12)
plot(fd)
acf(fd)
fd1<-diff(fd)
ari<-arima(fd,order=c(1,1,0),seasonal=list(order=c(0,1,1),period=6))

Box.test(ari$resid)
pacf(f12)
adf.test(f12)
adf.test(fonction_date)
pacf(fonction_date)
auto.arima(f12)
----------------------------
## ARMA
#install.packages("urca")
install.packages("forecast")
#install.packages("zoo")
library("urca")

library("zoo")
library("tseries")
library("forecast")
adf.test(r) 
## p-value = 0.01
## p-value < 0.05 on accepte H1 la serie est stationnaire
## stationary
acf(r)  
# MA = 1
pacf(r)

final.aic <- Inf
final.order <- c(0,0,0)
 for (i in 0:1) for (j in 0:2)  for (l in 0:2) for (m in 0:2) for (n in 0:2){
   current.aic <- AIC(arima(f12, order=c(i,j,0),seasonal=list(order=c(l,m,n),period=6)))
  
 if (current.aic < final.aic) {
     final.aic <- current.aic
    
 final.order <- c(l,m,n)
     final.arma <- arima(f12,order=c(i,j,k),seasonal=list(order=c(l,m,n)))
   }
}
auto.arima(f12)

AIC(arima(f12,order=c(2, 1, 2),seasonal=list(order=c(2, 0, 2))))
final.aic
final.order
ar1 = arima(f12,order=c(2, 1, 2),seasonal=list(order=c(2, 0, 2)))
summary(ar1)
model<-auto.arima(f12,max.p=10,max.q=10,max.P=10,max.d = 1,max.D=1,
                  max.Q=10,max.order=20,ic = "aicc", stepwise=FALSE, trace=TRUE,
                  test=c("kpss","adf","pp"),seasonal.test=c("ocsb","ch"))

Fit best ARIMA mode
ch.test(f12)
summary(model)



---------------------------------------------------------

## ARMA
#install.packages("urca")
#install.packages("zoo")
library("urca")
library("zoo")
library("tseries")
library("forecast")
adf.test(serie) 
## p-value = 0.3688
## p-value > 0.05 on accepte H1 la serie n'est pas stationnaire
pacf(f12)
final.aic <- Inf
final.order <- c(0,0,0)
 for (i in 0:1)  for(k in 0:2){
   current.aic <- AIC(arima(f12, order=c(i, k, 0)))


   if (current.aic < final.aic) {
     final.aic <- current.aic
     final.order <- c(i, k, 0)
     final.arma <- arima(f12, order=final.order)
   }
 }
final.aic
final.order
### SARIMA
final.aic <- Inf

final.seasonal <- c(0,0,0)
for (i in 0:2) for (j in 0:2) for(k in 0:2){
  current.aic <- AIC(arima(serie, order=c(2,1,2),seasonal = c(i,k,j)))
  if (current.aic < final.aic) {
    final.aic <- current.aic
    final.seosonal <- c(i, k, j)
  }
}
final.aic
final.seasonal
ndiffs(f12,test=c("kpss","adf", "pp"))
nsdiffs(fonction_date,test="ch")

final.order<-c (2,1,2)
?auto.sarima

k = auto.sarima(f12,stepwise = F, approximation = FALSE)
summary(k)
ARIMA(2,1,2)
sarma1 = arima(f12,order = c(2,1,2),seasonal = c(2,0,2))
summary(sarma1)
sarma2 = arima(f12,order = c(1,1,1),seasonal = c(0,2,2))
summary(sarma2)

sarma3 = arima(f12,order = c(1,1,1),seasonal=list(order=c(1,1,1),period=6))
AIC(sarma3)

AIC(arma2)

arma1 = arima(f12,order = c(2,1,2))
summary(arma1)
arma2 = arima(f12,order = c(1,1,1))
summary(arma2)


## predict
pred = predict(arma2,n.ahead=8)
pred_sarma2 = predict(sarma2,n.ahead=8)
plot(f12)
lines(pred$pred,col="blue", lwd=1)
lines(pred$pred+pred$se,col="red",lty=3, lwd=1)
lines(pred$pred-pred$se,col="red",lty=3, lwd=1)
library("forecast")
## forecast
fc <- forecast(sarma2)
fc1 <- forecast(arma1)
fc2 <- forecast(arma2,h=10)
fc3 <- forecast(sarma3)

plot(fonction_date)
plot(log(fonction_date))
lines(fc$fitted,col="yellow")
lines(fc1$fitted,col="red")
lines(fc2$fitted,col="green")
plot(fc2)
lines(fc3$fitted,col="blue")
lines(fc3$fitted)
?exp
bt = Box.test(resid(arma2),type="Ljung",lag=20,fitdf=0)
bt # p-value = 0.9305 # > 0.05 independant




Box.test(resid(ar1),type="Ljung",lag=20,fitdf=0)
qqnorm(resid(ar1))
bt # p-value = 0.9305 # > 0.05 independant
plot(serie)

