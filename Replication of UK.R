rm(list = ls())

graphics.off()

options(digits=4)

packages <- c("quantmod","fredr","ecb","tseries","Quandl","eurostat")
new.packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
invisible(lapply(packages, library, character.only = TRUE))

library(quantmod)

#Industrial Production for UK
GBRPROINDMISMEI <- getSymbols("GBRPROINDMISMEI", src = "FRED", from = "1979-06-01", auto.assign = FALSE)
GBRINDPRO <- window(GBRPROINDMISMEI, start ="1979-06-01",end = "1990-10-01")
head(GBRINDPRO)
tail(GBRINDPRO)

#Domestic Producer Price Index: Manufacturing for UK
GBRPPDMMINMEI <- getSymbols("GBRPPDMMINMEI", src = "FRED", from = "1979-06-01", auto.assign = FALSE)
GBRINDPRDEX <- window(GBRPPDMMINMEI, start ="1979-06-01",end = "1990-10-01")
head(GBRINDPRDEX)
tail(GBRINDPRDEX)

#Consumer Price Index for UK
GBRCPIALLMINMEI <- getSymbols("GBRCPIALLMINMEI", src = "FRED", from = "1979-06-01", auto.assign = FALSE)
GBRCPI <- window(GBRCPIALLMINMEI, start ="1979-06-01",end = "1990-10-01")
head(GBRCPI)
tail(GBRCPI)

#Consumer Price Inflation for UK
CPA.LTT01GBM659N <- getSymbols("CPALTT01GBM659N", src='FRED',from ="1974-01-01", auto.assign = FALSE)
GBR.CPA <- window(CPA.LTT01GBM659N,start="1974-01-01",end = "1990-10-01")
head(GBR.CPA)
tail(GBR.CPA)

#Short term interest rate for UK
INT.DSRGBM193N <- getSymbols("INTDSRGBM193N", src='FRED',from ="1974-01-01", auto.assign = FALSE)
GBR.SINTR <- window(INT.DSRGBM193N,start="1979-06-01",end = "1990-10-01")
head(GBR.SINTR)
tail(GBR.SINTR)

#Long term interest rate for UK
IRLTLT01GBM156N <- getSymbols("IRLTLT01GBM156N", src='FRED',from ="1974-01-01", auto.assign = FALSE)
GBR.LINTR <- window(IRLTLT01GBM156N,start="1979-06-01",end = "1990-10-01")
head(GBR.LINTR)
tail(GBR.LINTR)

#Call money
IRSTCI01GBM156N <- getSymbols("IRSTCI01GBM156N", src = "FRED", from = "1979-01-01", auto.assign = FALSE)
CALLMONEY_UK <- window(IRSTCI01GBM156N, start ="1979-06-01",end = "1990-10-01")
head(CALLMONEY_UK)
tail(CALLMONEY_UK)

#DM/Pound nominal rate
USUKFXUKM <- getSymbols("USUKFXUKM", src = "FRED", from = "1979-01-01", auto.assign = FALSE)
DOUK <- window(USUKFXUKM, start ="1979-06-01",end = "1990-10-01")
head(DOUK)
tail(DOUK)


#Switch to dataframe
ind.prod.uk <- data.frame(date = index(GBRINDPRO), coredata(GBRINDPRO))
cpi.uk <- data.frame(date = index(GBRCPI), coredata(GBRCPI))
sr_int.rate.uk <- data.frame(date = index(GBR.SINTR), coredata(GBR.SINTR))
lr_int.rate.uk <- data.frame(date = index(GBR.LINTR), coredata(GBR.LINTR))

#Finding y_t*
library(zoo)
ind.prod.uk.ts = ts(ind.prod.uk[, -1], frequency = 12, start=c(1979, 6, 1))
ind.prod.uk.ts = as.zoo(ind.prod.uk.ts)
head(ind.prod.uk.ts)
plot(ind.prod.uk.ts)

lg_ind.prod.uk <- log(ind.prod.uk.ts)

library(lmtest)
plot.ts(lg_ind.prod.uk)
t2 <- c(1:length(lg_ind.prod.uk))^2
trend <- lm(lg_ind.prod.uk~t2)

y_t.star <- residuals(trend)
plot.ts(y_t.star)

x_t.uk <- ind.prod.uk$GBRPROINDMISMEI-y_t.star
x_t.uk
length(x_t.uk)

#inflation
library(smooth)

##Rate pi_t and pi_t_n
GBRCPIALLMINMEI <- getSymbols("GBRCPIALLMINMEI", src = "FRED", from = "1979-06-01", auto.assign = FALSE)
GBRCPI <- window(GBRCPIALLMINMEI, start ="1979-06-01",end = "1990-10-01")
head(GBRCPI)
tail(GBRCPI)
cpi_uk <- GBRCPI$GBRCPIALLMINMEI
cpi_uk <- data.frame(date = index(GBRCPI), coredata(GBRCPI))
inflation_uk = cpi_uk$GBRCPIALLMINMEI

final_inflation_uk=c()
for (i in 1:length(inflation_uk)){
  final_inflation_uk[i]=((inflation_uk[i+12]-inflation_uk[i])/inflation_uk[i])*100
}

length(final_inflation_uk)
final_inflation_uk=na.exclude(final_inflation_uk)

#Expected Inflation
MA_Inflation_uk=SMA(final_inflation_uk, n=12,silent=FALSE)

#Finding x_t
library(zoo)
indpro_uk.ts = ts(ind.prod.uk$GBRPROINDMISMEI, frequency = 12, start=c(1979,4, 1))
indpro_uk.ts = as.zoo(indpro_uk.ts)
head(indpro_uk.ts)
plot(indpro_uk.ts)
lg_indpro_uk <- log(indpro_uk.ts)

library(lmtest)

t2=(c(1:length(lg_indpro_uk)))^2
trend <- lm(lg_indpro_uk~t2)

y_star2 <- residuals(trend)
plot.ts(y_star2)

output_index_uk = log(ind.prod.uk$GBRPROINDMISMEI)



x_t <- output_index_uk-y_star2
x_t
x_t.ts <- ts(x_t, frequency = 12, start=c(1979,4, 1))
x_t.xts <- as.xts(x_t.ts)

######################
## Lagged variables ##
######################

library(statsr)

#Lags of x_t
x_t_lag=matrix(ncol=12, nrow=137)
for (i in 1:12){
  x_t_lag[,i]=lag(x_t.xts, i)
}

#Lags of phi_t
fin_inflation_uk_data=as.data.frame.ts(final_inflation_uk)
fin_inflation_uk_matrix=as.matrix(fin_inflation_uk_data)
fin_inflation_uk_matrix=as.ts(fin_inflation_uk_matrix)
fin_inflation_uk_matrix=as.xts(fin_inflation_uk_matrix)
phi_lag=matrix(ncol=12, nrow=125)
for (i in 1:12){
  phi_lag[,i]=lag(fin_inflation_uk_matrix, i)
}

#Lags of log difference of commodity price index (o_t): in our case manufacturing
o_t <- diff(log(GBRINDPRDEX))
o_t_lag=matrix(ncol=12, nrow=137)
for (i in 1:12){
  o_t_lag[,i]=lag(o_t, i)
}


#Lags of call money (r_t)
r_t_lag = matrix(ncol=12, nrow=137)
for (i in 1:12){
  r_t_lag[,i] = lag(CALLMONEY_UK, i)
}

#Lags of  log difference of the dm/dollar real exchange rate (q_t)
q <- log(DOUK)
q_t <- diff(q)
length(q_t)
q_t_lag=matrix(ncol=12, nrow=137)
for (i in 1:12){
  q_t_lag[,i]=lag(q_t, i)
}










