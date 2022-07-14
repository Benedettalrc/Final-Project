rm(list = ls())

graphics.off()

options(digits=4)

#This paper reports estimates of monetary policy reaction functions for two countries:
#Japan and UK. We find that since 1979 the central bank of Japan has pursued an implicit form 
#of inflation targeting, which may account for the broad success of monetary policy in this 
#country over this time period.The evidence also suggests that this central bank has been
#forward looking: it responds to anticipated inflation as opposed to lagged inflation. 
#During the same period, the central Bank of UK was instead heavily influenced by German 
#monetary policy. Using the Bundesbank’s policy rule as a benchmark, we can see that at the time
#interest rates in UK were much higher than domestic macroeconomic conditions warranted.


#######################
## Download the data ##
#######################

packages <- c("quantmod","fredr","ecb","tseries","Quandl","eurostat")
new.packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
invisible(lapply(packages, library, character.only = TRUE))

library(quantmod)

JPNPROINDMISMEI <- getSymbols("JPNPROINDMISMEI", src='FRED',from ="1979-04-01", auto.assign = FALSE)
JPNINDPRO <- window(JPNPROINDMISMEI,start="1979-04-01",end = "1993-12-31")
head(JPNINDPRO)
tail(JPNINDPRO)


JPNCPIALLMINMEI <- getSymbols("JPNCPIALLMINMEI", src = "FRED", from ="1979-04-01", auto.assign = FALSE)
JPNCPI <- window(JPNCPIALLMINMEI,start="1979-04-01",end = "1993-12-31")
head(JPNCPI)
tail(JPNCPI)


GBRPROINDMISMEI <- getSymbols("GBRPROINDMISMEI", src = "FRED", from = "1979-06-01", auto.assign = FALSE)
GBRINDPRO <- window(GBRPROINDMISMEI, start ="1979-06-01",end = "1990-10-01")
head(GBRINDPRO)
tail(GBRINDPRO)

GBRCPIALLMINMEI <- getSymbols("GBRCPIALLMINMEI", src = "FRED", from = "1979-06-01", auto.assign = FALSE)
GBRCPI <- window(GBRCPIALLMINMEI, start ="1979-06-01",end = "1990-10-01")
head(GBRCPI)
tail(GBRCPI)

#################################################################################
## Plot the consumer price inflation versus short-term interest rate for JAPAN ##
#################################################################################

#There was a fundamental shift in the way these central banks conducted monetary policy, 
#beginning in each case some time during 1979.For each central bank, controlling 
#inflation became a major focus of monetary policy. 
#The vertical line in each panel denotes the approximate time each central
#bank changed the course of policy, for Japan we pick April 1979, a period of significant
#financial market deregulation.

CPALTT01JPM659N <- getSymbols("CPALTT01JPM659N", src='FRED',from ="1974-01-01", auto.assign = FALSE)
JPNCPA <- window(CPALTT01JPM659N,start="1974-01-01",end = "1993-12-31")
head(JPNCPA)
tail(JPNCPA)

INTDSRJPM193N <- getSymbols("INTDSRJPM193N", src='FRED',from ="1974-01-01", auto.assign = FALSE)
JPNINTR <- window(INTDSRJPM193N,start="1974-01-01",end = "1993-12-31")
head(JPNINTR)
tail(JPNINTR)

jpncpa_data <- data.frame(date=index(JPNCPA), coredata(JPNCPA))
jpnintr_data <- data.frame(date=index(JPNINTR), coredata(JPNINTR))
jpndata <- merge(jpnintr_data, jpncpa_data)
datejpn <- sort(jpndata$date)
datejpn[63]

library(ggplot2)

jpngraph <- ggplot(jpndata, aes(x=date)) + 
  geom_line(aes(y =  CPALTT01JPM659N), color = "darkred") + 
  geom_line(aes(y = INTDSRJPM193N), color="steelblue", linetype="twodash")

jpngraph + geom_vline(xintercept = datejpn[63], color = "red")


#################################################################################
##  Plot the consumer price inflation versus short-term interest rate for UK   ##
#################################################################################

#The vertical line in each panel denotes the approximate time each central
#bank changed the course of policy, the starting point for the Bank of England is 
#June 1979, the date that Thatcher assumed power, and fighting inflation became a 
#clear policy objective. 

CPALTT01GBM659N <- getSymbols("CPALTT01GBM659N", src='FRED',from ="1974-01-01", auto.assign = FALSE)
GBRCPA <- window(CPALTT01GBM659N,start="1974-01-01",end = "1993-12-31")
head(GBRCPA)
tail(GBRCPA)

INTDSRGBM193N <- getSymbols("INTDSRGBM193N", src='FRED',from ="1974-01-01", auto.assign = FALSE)
GBRINTR <- window(INTDSRGBM193N,start="1974-01-01",end = "1993-12-31")
head(GBRINTR)
tail(GBRINTR)

gbrcpa_data <- data.frame(date=index(GBRCPA), coredata(GBRCPA))
gbrintr_data <- data.frame(date=index(GBRINTR), coredata(GBRINTR))
gbrdata <- merge(gbrintr_data, gbrcpa_data)
dategbr <- sort(gbrdata$date)
dategbr[63]

library(ggplot2)

gbrgraph <- ggplot(gbrdata, aes(x=date)) + 
  geom_line(aes(y =  CPALTT01GBM659N), color = "darkred") + 
  geom_line(aes(y = INTDSRGBM193N), color="steelblue", linetype="twodash")

gbrgraph + geom_vline(xintercept = dategbr[63], color = "red")


#######################
## Table 1 for Japan ##
#######################

## Yen to dollar real spot exchange 
DEXJPUS <- getSymbols("DEXJPUS", src ='FRED', from ="1978-04-01", auto.assign = FALSE)
DEXJPUS <- window(DEXJPUS, start ="1979-04-01",end = "1993-12-31")
head(DEXJPUS)
tail(DEXJPUS)

## Immediate rate less than 24 hours, Central Bank of Japan
IRSTCB01JPM156N <- getSymbols("IRSTCB01JPM156N", src='FRED',from ="1979-04-01", auto.assign = FALSE)
IRSTCB01JPM156N <- window(IRSTCB01JPM156N,start="1979-04-01",end = "1993-12-31")
head(IRSTCB01JPM156N)
tail(IRSTCB01JPM156N)

## Producer price index for Japan
PITGCG01JPM661N <- getSymbols("PITGCG01JPM661N", src='FRED',from ="1979-04-01", auto.assign = FALSE)
PRODEX <- window(IRSTCB01JPM156N,start="1979-04-01",end = "1993-12-31")
head(PRODEX)
tail(PRODEX)

##########################
## Switch to data-frame ##
##########################

indpro_jnp <- data.frame(date = index(JPNINDPRO), coredata(JPNINDPRO))
cpi_jnp <- data.frame(date = index(JPNCPI), coredata(JPNCPI))
r.ex.rate_jnp <- data.frame(date = index(DEXJPUS), coredata(DEXJPUS))
r.ex.rate_jnp <- na.exclude(r.ex.rate_jnp)
immrate_jnp <- data.frame(date = index(IRSTCB01JPM156N), coredata(IRSTCB01JPM156N))
prodex_jnp <- data.frame(date = index(PRODEX), coredata(PRODEX))

#############################
## Create lagged variables ##
#############################

#Lags of y_t
lag12_indpro_jnp <- lags(indpro_jnp, 12)

#Lags of phi_t
lag12_cpi_jnp <- lags(cpi_jnp, 12)

#Lags of the log difference of the producer price index
dflg_proind_jnp <- diff(log(prodex_jnp[,2]))
lag12_dflg_proind_jnp <- lags(dflg_proind_jnp, 12)

#Lags of the day-to-day rate
lag12_immrate_jnp <- lags(immrate_jnp, 12)

#Lags of the log difference of q_t
dflg_r.ex.rate_jnp <- diff(log(r.ex.rate_jnp[,2]))
lag12_dflg_r.ex.rate_jnp <- lags(dflg_r.ex.rate_jnp, 12)

##################
## Compute y*_t ##
##################

## Quadratic detrend of the log of industrial production to find x_t

library(zoo)
indpro_jnp.ts = ts(indpro_jnp[, -1], frequency = 12, start=c(1979, 4, 1))
indpro_jnp.ts = as.zoo(indpro_jnp.ts)
head(indpro_jnp.ts)
plot(indpro_jnp.ts)

lg_indpro_jnp <- log(indpro_jnp.ts)

library(lmtest)
plot.ts(lg_indpro_jnp)
t2=c(1:length(lg_indpro_jnp))^2
trend <- lm(lg_indpro_jnp~t2)

y_star2 <- residuals(trend)
plot.ts(y_star2)

x_t <- indpro_jnp$JPNPROINDMISMEI-y_star2
x_t
length(x_t)

##############
## Baseline ##
##############

##Forecast Inflation##
library(forecast)
forecast_Inflatio = naive(y = GBRCPA$CPALTT01GBM659N, h = 12)
forecast_Inflation$x
forecast_Inflation2=as.data.frame(forecast_Inflation)
forecast_Inflation2$`Point Forecast`
forecast_Inflation3 = as.data.frame.ts(forecast_Inflation2$`Point Forecast`)
merge(forecast_Inflation$x , forecast_Inflation3$x)




library(smooth)

MA_Inflation = SMA(cpi_jnp$JPNCPIALLMINMEI, n=12,silent=FALSE)

lag1_rate_japan = as.data.frame(lag1_immrate_jnp)
lag1_rate_japan$IRSTCB01JPM156N.1

inflation_jp = MA_Inflation

#rate lag 1
r1 <- rbind.fill.matrix(lag1_rate_japan$IRSTCB01JPM156N.1,NA )
r1 <- as.numeric(r1)
lm(immrate_jnp$IRSTCB01JPM156N~inflation_jp+x_t+r1)


##first step
install.packages("pls")


library(pls)

model <- plsr(immrate_jnp$IRSTCB01JPM156N~inflation_jp+x_t, scale=TRUE, validation="CV")
summary(model)



INT.DSRJPM193N <- getSymbols("INTDSRJPM193N", src='FRED',from ="1979-04-01", auto.assign = FALSE)
JPN.INTR <- window(INTDSRJPM193N,start="1979-04-01",end = "1993-12-31")
head(JPN.INTR)
tail(JPN.INTR)

jpn.intr_data <- data.frame(date=index(JPN.INTR), coredata(JPN.INTR))









