rm(list = ls())

graphics.off()

options(digits=4)

packages <- c("quantmod","fredr","ecb","tseries","Quandl","eurostat")
new.packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
invisible(lapply(packages, library, character.only = TRUE))

library(quantmod)

#Idustrial Production for UK
GBRPROINDMISMEI <- getSymbols("GBRPROINDMISMEI", src = "FRED", from = "1979-06-01", auto.assign = FALSE)
GBRINDPRO <- window(GBRPROINDMISMEI, start ="1979-06-01",end = "1990-10-01")
head(GBRINDPRO)
tail(GBRINDPRO)

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

#Short term interest rate UK
INT.DSRGBM193N <- getSymbols("INTDSRGBM193N", src='FRED',from ="1974-01-01", auto.assign = FALSE)
GBR.INTR <- window(INT.DSRGBM193N,start="1974-01-01",end = "1990-10-01")
head(GBR.INTR)
tail(GBR.INTR)
immrate_uk=as.data.frame(GBR.INTR )

#Long term interest rate for UK
IRLTLT01GBM156N <- getSymbols("IRLTLT01GBM156N", src='FRED',from ="1974-01-01", auto.assign = FALSE)
GBR.LINTR <- window(IRLTLT01GBM156N,start="1979-06-01",end = "1990-10-01")
head(GBR.LINTR)
tail(GBR.LINTR)


##CALL MONEY

IRSTCI01GBM156N<- getSymbols("IRSTCI01GBM156N", src = "FRED", from = "1978-01-01", auto.assign = FALSE)
CALLMONEY_UK <- window(IRSTCI01GBM156N, start ="1978-01-01",end = "1990-10-01")
head(CALLMONEY_UK)
tail(CALLMONEY_UK)



#Base Line per UK



#inflation
library(smooth)

##Rate pi_t and pi_t_n

GBRCPIALLMINMEI <- getSymbols("GBRCPIALLMINMEI", src = "FRED", from = "1979-06-01", auto.assign = FALSE)
GBRCPI <- window(GBRCPIALLMINMEI, start ="1979-06-01",end = "1990-10-01")
head(GBRCPI)
tail(GBRCPI)
cpi_uk <- data.frame(date = index(GBRCPI), coredata(GBRCPI))
inflation_uk=cpi_uk$GBRCPIALLMINMEI

final_inflation_uk=c()
for (i in 1:length(inflation_uk)){
  final_inflation_uk[i]=((inflation_uk[i+12]-inflation_uk[i])/inflation_uk[i])*100
}

length(final_inflation_uk)
final_inflation_uk=na.exclude(final_inflation_uk)
#Expected Inflation
MA_Inflation_uk=SMA(final_inflation_uk, n=12,silent=FALSE)

## Quadratic detrend of the log of industrial production to find x_t

INTDSRGBM193N <- getSymbols("INTDSRGBM193N", src='FRED',from ="1974-01-01", auto.assign = FALSE)
GBRINTR <- window(INTDSRGBM193N,start="1974-01-01",end = "1993-12-31")
head(GBRINTR)
tail(GBRINTR)
indpro_uk=as.data.frame(GBRINTR)


library(zoo)
indpro_uk.ts = ts(indpro_uk$INTDSRGBM193N, frequency = 12, start=c(1979,4, 1))
indpro_uk.ts = as.zoo(indpro_uk.ts)
head(indpro_uk.ts)
plot(indpro_uk.ts)
lg_indpro_uk <- log(indpro_uk.ts)

library(lmtest)

t2=(c(1:length(lg_indpro_uk)))^2
trend <- lm(lg_indpro_uk~t2)

y_star2 <- residuals(trend)
plot.ts(y_star2)

output_index_uk=log(indpro_uk$INTDSRGBM193N)



x_t <- output_index_uk-y_star2
x_t
##x_t expected
expec_Xt=SMA(x_t,n=12,silent=FALSE)


# #Expected output
MA_output=SMA(output_index_uk, n=12,silent=FALSE)

#Expected output - output star
expXt= MA_output[1:125]-y_star2[1:125]
#Expected inflation - inflation star
expInf= MA_Inflation_uk-2

##Long Run rate UK (get the variable)
#Long term interest rate for UK

longrunN_uk.2 <- data.frame(date=index(GBR.LINTR), coredata(GBR.LINTR))


###Check the means 
mean(as.numeric(longrunN_uk.2$IRLTLT01GBM156N))
mean(na.exclude(MA_Inflation_uk))
mean(final_inflation_uk)

mean(na.exclude(expInf))
mean(na.exclude(MA_output[1:125]))
mean(output_index_uk)


#Equation 2.1
#Not include intercept, the long run variable has coefficient 1, thats ok

glm(CALLMONEY_UK$IRSTCI01GBM156N[13:137]~0+longrunN_uk.2$IRLTLT01GBM156N[1:125]+expInf[1:125]+expXt)
#method1.2 with final_inflation_uk
final_inflation_uk=final_inflation_uk[1:125]-2
glm(CALLMONEY_UK$IRSTCI01GBM156N[13:137]~0+longrunN_uk.2$IRLTLT01GBM156N[1:125]+final_inflation_uk+expXt)

#Second method when yt=0 use yt star

glm(CALLMONEY_UK$IRSTCI01GBM156N[13:137]~0+longrunN_uk.2$IRLTLT01GBM156N[1:125]+expInf[1:125]+y_star2[1:125])
#method 2.1 final_inflation_uk

glm(CALLMONEY_UK$IRSTCI01GBM156N[13:137]~0+longrunN_uk.2$IRLTLT01GBM156N[1:125]+final_inflation_uk[1:125]+y_star2[1:125])
#Third method use the variable output_index_uk

glm(CALLMONEY_UK$IRSTCI01GBM156N[13:137]~0+longrunN_uk.2$IRLTLT01GBM156N[1:125]+expInf[1:125]+output_index_uk[1:125])

#method 3.1 output_index_uk
glm(CALLMONEY_UK$IRSTCI01GBM156N[13:137]~0+longrunN_uk.2$IRLTLT01GBM156N[1:125]+final_inflation_uk[1:125]+output_index_uk[1:125])

#fourth method the variable output without the log indpro_uk$INTDSRGBM193N

glm(CALLMONEY_UK$IRSTCI01GBM156N[13:137]~0+longrunN_uk.2$IRLTLT01GBM156N[1:125]+expInf[1:125]+indpro_uk$INTDSRGBM193N[1:125])
#method 4.1 output_index_uk

glm(CALLMONEY_UK$IRSTCI01GBM156N[13:137]~0+longrunN_uk.2$IRLTLT01GBM156N[1:125]+output_index_uk[1:125]+indpro_uk$INTDSRGBM193N[1:125])

#Fifth method with the expectation of indpro_uk$INTDSRGBM193N
expectINPRO_UK= SMA(indpro_uk$INTDSRGBM193N, n=12)
glm(CALLMONEY_UK$IRSTCI01GBM156N[13:137]~0+longrunN_uk.2$IRLTLT01GBM156N[1:125]+expInf[1:125]+expectINPRO_UK[1:125])
#method 5.1
glm(CALLMONEY_UK$IRSTCI01GBM156N[13:137]~0+longrunN_uk.2$IRLTLT01GBM156N[1:125]+output_index_uk[1:125]+expectINPRO_UK[1:125])


##Equation 2.3




#Equation 2.4
#Here we used the expected inflation and expected output gap without do the rest 

glm(CALLMONEY_UK$IRSTCI01GBM156N[13:137]~MA_Inflation_uk+expec_Xt[1:125])

#Second method when yt=0 use yt star

glm(CALLMONEY_UK$IRSTCI01GBM156N[13:137]~MA_Inflation_uk+y_star2[1:125])


#Third method use the variable output_index_uk
glm(CALLMONEY_UK$IRSTCI01GBM156N[13:137]~MA_Inflation_uk+output_index_uk[1:125])

#fourth method the variable output without the log indpro_uk$INTDSRGBM193N
glm(CALLMONEY_UK$IRSTCI01GBM156N[13:137]~MA_Inflation_uk+indpro_uk$INTDSRGBM193N[1:125])
#Fifth method with the expectation of indpro_uk$INTDSRGBM193N
glm(CALLMONEY_UK$IRSTCI01GBM156N[13:137]~MA_Inflation_uk+expectINPRO_UK[1:125])




#Equation 2.5

r.1=CALLMONEY_UK$IRSTCI01GBM156N[14:138]

glm(CALLMONEY_UK$IRSTCI01GBM156N[13:137]~r.1[1:125]+MA_Inflation_uk+expec_Xt[1:125])

#Second method when yt=0 use yt star
glm(CALLMONEY_UK$IRSTCI01GBM156N[13:137]~r.1[1:125]+MA_Inflation_uk+y_star2[1:125])

#Third method use the variable output_index_uk
glm(CALLMONEY_UK$IRSTCI01GBM156N[13:137]~r.1[1:125]+MA_Inflation_uk+output_index_uk[1:125])

#fourth method the variable output without the log indpro_uk$INTDSRGBM193N
glm(CALLMONEY_UK$IRSTCI01GBM156N[13:137]~r.1[1:125]+MA_Inflation_uk+indpro_uk$INTDSRGBM193N[1:125])
#Fifth method with the expectation of indpro_uk$INTDSRGBM193N

glm(CALLMONEY_UK$IRSTCI01GBM156N[13:137]~r.1[1:125]+MA_Inflation_uk+expectINPRO_UK[1:125])








##########USING FOURTH METHOD WITH IV REGRESSION###############

#############################
## Create lagged variables ##
#############################
##Lags 
#Lags of y_t
indprod.uk_lag=matrix(ncol=12,nrow=137)
library(statsr)
for (i in 1:12){
  indprod.uk_lag[,i]=lag(GBRINDPRO, i)
}

indprod.uk_lag=as.data.frame(indprod.uk_lag[1:125,])




#Equation 2.5

length(indprod.uk_lag$V1)
library(ivreg)

ivreg(CALLMONEY_UK$IRSTCI01GBM156N[13:137]~r.1[1:125]+MA_Inflation_uk+indpro_uk$INTDSRGBM193N[1:125]
      |indpro_uk$INTDSRGBM193N[1:125]+indprod.uk_lag$V1+indprod.uk_lag$V2
      +indprod.uk_lag$V3+indprod.uk_lag$V4+indprod.uk_lag$V5
      +indprod.uk_lag$V6+indprod.uk_lag$V9+indprod.uk_lag$V12)