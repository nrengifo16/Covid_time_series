rm(list=ls())
library(tidyverse)
library(readxl)
library(fUnitRoots)
library(tseries)
library(forecast)
library(MASS)
library(timeSeries)
library(feasts)
library(quantmod)
library(rugarch)
library(car)
library(FinTS)
library(PerformanceAnalytics)
library(fGarch)
library(ggplot2)
library(geoR)
library(BIGL)

# Casos
casos <- read_excel("C:/Users/nreng/OneDrive/Documentos/R/PI1/casos.xlsx", sheet = "Colombia", col_types = c("skip",
                                                                         "numeric"))
num_pred <- 10
num_elem <- length(casos)
casos <- ts(casos)
data <- head(casos,num_elem - num_pred)
real <- tail(casos,num_pred)

plot(data,type= "l")
acf(data)
pacf(data)

unitrootTest(data)
pp.test(data)
kpss.test(data)

# tenemos que diferenciar
diff_data = diff(data,1)
plot(diff_data,type= "l")
acf(diff_data)
pacf(diff_data)
unitrootTest(diff_data)
pp.test(diff_data)
kpss.test(diff_data)

#### LN ######
logdata = log(data)
plot(logdata,type= "l")

# Sacamos diferencias del logaritmo
diff_logdata = diff(logdata,2) # tomando todos los datos
plot(diff_logdata,type= "l")
acf(diff_logdata)
pacf(diff_logdata)
unitrootTest(diff_logdata)
pp.test(diff_logdata)
kpss.test(diff_logdata)

# Alternative
Mdl1 <- arima(logdata,order = c(6,2,0))
Mdl2 <- arima(logdata,order = c(14,2,0))
Mdl4 <- auto.arima(logdata, stepwise = FALSE, d = 2)

##### Box Cox ####
bc <- boxcox(data ~ 1)
(l1 <- bc$x[which.max(bc$y)])
bc_data <- BoxCox(data,lambda = l1)

plot(bc_data,type= "l")

# Difference
diff_bcdata = diff(bc_data,1)
plot(diff_bcdata,type= "l")
acf(diff_bcdata)
pacf(diff_bcdata)
unitrootTest(diff_bcdata)
pp.test(diff_bcdata)
kpss.test(diff_bcdata)

Mdl3 <- arima(bc_data,order = c(6,2,2))

### GARCH ####
plot(diff_bcdata,type= "l")

garch11.spec = ugarchspec(variance.model = list(garchOrder=c(1,1)), 
                          mean.model = list(armaOrder=c(1,0)))

Mdl5 <- ugarchfit(spec=garch11.spec, data=diff_data)

### Validation ####
a <- unitrootTest(diff(logdata,2))
b <- pp.test(diff(logdata,2))
c <- kpss.test(diff(logdata,2))
a@test[["p.value"]][["t"]]
b[["p.value"]]
c[["p.value"]]

a1 <- unitrootTest(diff(bc_data,2))
b1 <- pp.test(diff(bc_data,2))
c1 <- kpss.test(diff(bc_data,2))
a1@test[["p.value"]][["t"]]
b1[["p.value"]]
c1[["p.value"]]

Mdl_aux <- Mdl5
m1 <- Box.test(Mdl_aux[["residuals"]], lag = 12, type = "Ljung", fitdf = 8)
m2 <- white.test(Mdl_aux[["residuals"]])
m1[["p.value"]]
m2[["p.value"]]

#### Forecast ####

preEst1 <- forecast(Mdl1,num_pred)
preEst2 <- forecast(Mdl2,num_pred)
preEst3 <- forecast(Mdl3,num_pred)
preEst4 <- forecast(Mdl4,num_pred)
preEst5 <- ugarchforecast(Mdl5,n.ahead = num_pred)

Est1 <- exp(preEst1$mean)
Est2 <- exp(preEst2$mean)
Est3 <- InvBoxCox(preEst3[["mean"]], lambda = l1)
Est4 <- exp(preEst4$mean)
Est5 <- last(data)+cumsum(preEst5@forecast[["seriesFor"]])

# Plots

Est1 <- as.numeric(Est1)
Est2 <- as.numeric(Est2)
Est3 <- as.numeric(Est3)
Est4 <- as.numeric(Est4)
Est5 <- as.numeric(Est5)
real <- as.numeric(real)

makePlot()
plot(Est1,type="l",col="red")
lines(Est2,col="green")
lines(Est3,col="orange")
lines(Est4,col="blue")
lines(Est5,col="yellow")
lines(real,col="black")
legend("topleft", legend=c("LN ARIMA(6,2,0)", "LN ARIMA(14,2,0)",
       "BC ARIMA(6,2,2)","AUTO ARIMA(6,2,2)","Dif GARCH(1,1)","Real"),
       col=c("red","green","orange","blue","yellow","black"), lty=1:2, cex=0.8)

# Error %
e1 <- abs(real-Est1)/real
e2 <- abs(real-Est2)/real
e3 <- abs(real-Est3)/real
e4 <- abs(real-Est4)/real
e5 <- abs(real-Est5)/real
(cbind(e1,e2,e3,e4,e5))


### Auxiliar ####

data_aux = diff(bc_data,2)
# data_aux = bc_data"
par(mfrow = c(3,3))
plot(data_aux,type="l",col="blue")
acf(data_aux)
pacf(data_aux)





