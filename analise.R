library(tidyverse)

desocupacao <- readr::read_csv("ipeadata[23-01-2023-02-08].csv")
desocupacao$taxa_desocupacao<-desocupacao$`Taxa de desocupação - (%) - Instituto Brasileiro de Geografia e Estatística- Pesquisa Nacional por Amostra de Domicílios Contínua (IBGE/PNAD Contínua) - PNADC12_TDESOC12`/100
desocupacao1 <- desocupacao |> 
  select(taxa_desocupacao)
# write_csv(desocupacao1,'taxa_desocupacao.csv')
# write_csv(desocupacao1[1:117,],'taxa_desocupacao2.csv')
# writexl::write_xlsx(desocupacao1,'taxa_desocupacao.xlsx')
# 2012.03 até 2022.11


require(forecast)
require(tseries)
require(TSA)
require(descomponer)
require(moments)
require(stats)
require(astsa)

Y <- ts(desocupacao1$taxa_desocupacao,start=c(2012,3), frequency = 12)
plot.ts(Y)
n<-length(Y)
summary(y)


plot(stl(Y,s.window = 12))
# number of forecast steps
h1 <- 12

# Taking off the last 12 observations
n<-n-h1

y<-ts(Y[1:n], start=c(2012,3), frequency = 12)
m=length(Y)
y_obs=Y[(n+1):m] 

# some graphics
plot(y)
monthplot(y)
acf(y)
pacf(y)
adf.test(y)



# x<-1:length(Y)
# y.loess<-loess(Y~x,span=0.06,data=data.frame(x=x,y=Y),
#                control = loess.control(surface = "direct"))
# #C?lculo Predi??o
# tmp<-predict(y.loess,data.frame(x= x,y=Y),se=T)
# 
# tmp$fit

# 3
# dif1<- (y*(1:n)/12)^9
# dif2<- (y_obs*((n+1):(n+h1))/12)^9

# 1

# dif1<- cbind((sin(2*pi*(1:n)/12))) 
# dif2<- cbind((sin(2*pi*((n+1):(n+h1))/12)))

dif1<- cbind((sin(2*pi*(1:n)/12)),12*(sin(2*pi*(1:n)/120))) 
dif2<- cbind((sin(2*pi*((n+1):(n+h1))/12)),12*(sin(2*pi*((n+1):(n+h1))/120)))


# Auto sarima
mod<-auto.arima(y,xreg=dif1,stationary = T)
pred<-predict(mod,n.ahead=12,newxreg = dif2)
# teste<-Box.test(res, lag = 103, type = c("Ljung-Box"), fitdf = 0)
# teste$p.value
# shapiro.test(res)
# adf.test(res)
# ggpubr::ggqqplot(as.vector(res))

# Beta
source("barma.r")
source("best.barma.r")

# best<-best.barma(y,sf=c(start=c(2012, 3),frequency=12),pmax=5, qmax=5)
# best<-best.barma(y,sf=c(start=c(2012, 3),frequency=12),pmax=5, qmax=5,X = dif1,X_hat = dif2)
# 
# barma2<-barma(y,ar=c(1,2,3,4), ma=c(1,2,3,4,6), h=h1,diag=1,resid=1,X = dif1,X_hat = dif2)# 2

barma2<-barma(y,ar=c(1,2,4,5), ma=c(1,2,6), h=h1,diag=1,resid=1,X = dif1,X_hat = dif2)# 1

# barma2<-barma(y,ar=c(1,2,3,4,5), ma=c(1,2,9,12), h=h1,diag=1,resid=1,X = dif1,X_hat = dif2)# 3

# barma2<-barma(y,ar=c(1), ma=c(3,12,15), h=h1,diag=1,resid=1)

# barma2<-barma(y,ar=c(1,2,3), ma=c(1,12,15), h=h1,diag=1,resid=1)

plot(barma2$forecast)
res=barma2$resid1
pref=barma2$forecast
teste<-Box.test(res, lag = 110, type = c("Ljung-Box"), fitdf = 0)
teste$p.value
shapiro.test(res)
adf.test(res)
ggpubr::ggqqplot(as.vector(res))


# ---------------------------------------------------------------------------

############### EQM #################
residuos_beta_prev =(y_obs-pref)
residuos_sarima_prev = (y_obs-as.vector(pred$pred))


eqm_bsarma_prev = (sum(residuos_beta_prev^2))/length(residuos_beta_prev)
eqm_sarima_prev = (sum(residuos_sarima_prev^2))/length(residuos_sarima_prev)

############### MAPE #################

mape_bsarma_prev = sum( abs(residuos_beta_prev)/abs(y_obs) )/ length(residuos_beta_prev)
mape_sarima_prev = sum( abs(residuos_sarima_prev)/abs(y_obs) )/ length(residuos_sarima_prev)

eqm=c(eqm_bsarma_prev, eqm_sarima_prev)
mape=c(mape_bsarma_prev, mape_sarima_prev)

matriz=cbind(eqm, mape)
rownames(matriz)=c("BARMA", "ARIMA")
matriz


plot(y_obs,ylim = c(0.05,0.13))
lines(y_obs,col=1)
lines(pref,col=2)
lines(as.vector(pred$pred),col=4)

