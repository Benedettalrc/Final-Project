rm(list = ls())
graphics.off()
options(digits=4)

packages <- c("quantmod","fredr","ecb","tseries","Quandl","eurostat")
new.packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
invisible(lapply(packages, library, character.only = TRUE))

library(quantmod)

#############
## GERMANY ##
#############

#Industrial Production for DE
DEUPROINDMISMEI <- getSymbols("DEUPROINDMISMEI", src = "FRED", from = "1979-04-01", auto.assign = FALSE)
DE.INDPRO <- window(DEUPROINDMISMEI, start ="1979-04-01",end = "1993-12-01")
head(DE.INDPRO)
tail(DE.INDPRO)

#Industrial production index for DE
#A018ADDEA338NNBR <- getSymbols("A018ADDEA338NNBR", src = "FRED", from = "1979-04-01", auto.assign = FALSE)
#DEINDEX.INDPRO <- window(A018ADDEA338NNBR, start ="1979-04-01",end = "1993-12-01")
#head(DEINDEX.INDPRO)
#tail(DEINDEX.INDPRO)

#Consumer Price Inflation for DE
CPA.LTT01DEM659N <- getSymbols("CPALTT01DEM659N", src='FRED',from ="1979-04-01", auto.assign = FALSE)
DECPA <- window(CPA.LTT01DEM659N,start="1979-04-01",end = "1993-12-01")
head(DECPA)
tail(DECPA)

#Consumer Price Index for DE 
DEUCPIALLMINMEI <- getSymbols("DEUCPIALLMINMEI", src = "FRED", from = "1979-04-01", auto.assign = FALSE)
DECPI <- window(DEUCPIALLMINMEI, start ="1979-04-01",end = "1993-12-01")
head(DECPI)
tail(DECPI)

#Short term interest rate for DE (less than 24 hours - call money)
IR.STCI01DEM156N <- getSymbols("IRSTCI01DEM156N", src='FRED',from ="1979-04-01", auto.assign = FALSE)
DESINTR <- window(IR.STCI01DEM156N,start="1979-04-01",end = "1993-12-01")
head(DESINTR)
tail(DESINTR)

#Long term interest rate for DE
IRLTLT01DEM156N <- getSymbols("IRLTLT01DEM156N", src='FRED',from ="1979-04-01", auto.assign = FALSE)
DE.LINTR <- window(IRLTLT01DEM156N,start="1979-04-01",end = "1993-12-01")
head(DE.LINTR)
tail(DE.LINTR)

# Domestic Producer Prices Index: Manufacturing for Germany
DEUPPDMMINMEI <- getSymbols("DEUPPDMMINMEI", src='FRED',from ="1979-04-01", auto.assign = FALSE)
DEINDPRTEX <- window(DEUPPDMMINMEI,start="1979-04-01",end = "1993-12-01")
head(DEINDPRTEX)
tail(DEINDPRTEX)

#US Dollar to National Currency Spot Exchange Rate for Germany 
CCUSSP01DEM650N <- getSymbols("CCUSSP01DEM650N", src='FRED',from ="1979-04-01", auto.assign = FALSE)
SPOTEXCRATE <- window(CCUSSP01DEM650N,start="1979-04-01",end = "1993-12-01")
head(SPOTEXCRATE)
tail(SPOTEXCRATE)

## Graph of DE (Interest rates and Inflation) ##

# INFLATION (CPI) for the graph (we chose a different larger time period)
CPALTT01DEM659N <- getSymbols("CPALTT01DEM659N", src='FRED',from ="1974-03-01", auto.assign = FALSE)
DE.CPA <- window(CPALTT01DEM659N,start="1974-03-01",end = "1993-12-01")
head(DE.CPA)
tail(DE.CPA)

# Short term interest rate for the graph (we chose a different larger time period)
IRSTCI01DEM156N <- getSymbols("IRSTCI01DEM156N", src='FRED',from ="1974-03-01", auto.assign = FALSE)
DE.SINTR <- window(IRSTCI01DEM156N,start="1974-03-01",end = "1993-12-01")
head(DE.SINTR)
tail(DE.SINTR)

# Switch to dataframe 
de.cpa <- data.frame(date=index(DE.CPA), coredata(DE.CPA))
de.sintr <- data.frame(date=index(DE.SINTR), coredata(DE.SINTR))
de.data <- merge(de.cpa, de.sintr)
date.de <- sort(de.data$date)
date.de[61]

library(ggplot2)

de.graph <- ggplot(de.data, aes(x = date)) + 
  geom_line(aes(y = CPALTT01DEM659N ), color="steelblue", linetype="twodash") + 
  geom_line(aes(y = IRSTCI01DEM156N), color="darkred")

de.graph + geom_vline(xintercept = date.de[61], color = "red")


####################
# Base Line for DE #
####################

#Inflation
library(smooth)

#Rate pi_t and pi_t_n

DEUCPIALLMINMEI <- getSymbols("DEUCPIALLMINMEI", src = "FRED", from = "1979-04-01", auto.assign = FALSE)
DECPI <- window(DEUCPIALLMINMEI, start ="1979-04-01",end = "1993-12-01")
head(DECPI)
tail(DECPI)
cpi_de <- data.frame(date = index(DECPI), coredata(DECPI))
inflation_de=cpi_de$DEUCPIALLMINMEI

final_inflation_de=c()
for (i in 1:length(inflation_de)){
  final_inflation_de[i]=((inflation_de[i+12]-inflation_de[i])/inflation_de[i])*100
}

length(final_inflation_de)
final_inflation_de=na.exclude(final_inflation_de)

#Lags of phi_t
fin_inflation_de_data=as.data.frame.ts(final_inflation_de)
fin_inflation_de_matrix=as.matrix(fin_inflation_de_data)
fin_inflation_de_matrix=as.ts(fin_inflation_de_matrix)
fin_inflation_de_matrix=as.xts(fin_inflation_de_matrix)
phi_lag=matrix(ncol=12, nrow=165)
for (i in 1:12){
  phi_lag[,i]=lag(fin_inflation_de_matrix, i)
}
phi_lag=as.data.frame(phi_lag)
length(phi_lag$V1)

# Expected Inflation for DE
MA_Inflation_de=SMA(final_inflation_de, n=12,silent=FALSE)
MA_Inflation_de


## Quadratic detrend of the log of industrial production to find x_t

DEUPROINDMISMEI <- getSymbols("DEUPROINDMISMEI", src = "FRED", from = "1979-04-01", auto.assign = FALSE)
DE.INDPRO <- window(DEUPROINDMISMEI, start ="1979-04-01",end = "1993-12-01")
head(DE.INDPRO)
tail(DE.INDPRO)
indpro_de=as.data.frame(DE.INDPRO)

library(zoo)
indpro_de.ts = ts(indpro_de$DEUPROINDMISMEI, frequency = 12, start=c(1979,4, 1))
indpro_de.ts = as.zoo(indpro_de.ts)
head(indpro_de.ts)
plot(indpro_de.ts)
lg_indpro_de <- log(indpro_de.ts)


library(lmtest)

t2=(c(1:length(lg_indpro_de)))^2
trend <- lm(lg_indpro_de~t2)

y_star2 <- residuals(trend)
plot.ts(y_star2)
length(y_star2)

output_index_de=log(indpro_de$DEUPROINDMISMEI)

x_t <- output_index_de-y_star2
x_t
length(x_t)

#Lag output gap
x_t.ts <- ts(x_t, frequency = 12, start=c(1979,4, 1))
x_t.xts <- as.xts(x_t.ts)
length(x_t.xts)

x_t.xts<- x_t.xts[1:177]

#Lags of x_t
x_t_lag=matrix(ncol=12, nrow=177)

for (i in 1:12){
  x_t_lag[,i]=lag(x_t.xts, i)
}

x_t_lag=as.data.frame(x_t_lag)
length(x_t_lag$V1)

# Expected x_t 
expec_Xt=SMA(x_t,n=12,silent=FALSE)
expec_Xt
length(expec_Xt)

# Expected output
MA_output=SMA(output_index_de, n=12,silent=FALSE)
length(MA_output)

# Expected output - output star
expXt= MA_output[1:177]-y_star2[1:177]
expXt
length(expXt)

# Expected inflation - inflation star
expInf= MA_Inflation_de-2
expInf
length(expInf)

# Long Run rate DE (get the variable)
IRLTLT01DEM156N <- getSymbols("IRLTLT01DEM156N", src='FRED',from ="1979-04-01", auto.assign = FALSE)
DE.LINTR <- window(IRLTLT01DEM156N,start="1979-04-01",end = "1993-12-01")
head(DE.LINTR)
tail(DE.LINTR)
longrun_de <- data.frame(date=index(DE.LINTR), coredata(DE.LINTR))
length(longrun_de)

################
# Equation 2.1 #
################

# Do not include the intercept, the long run variable has coefficient 1, that's ok
#fourth method the variable output without the log indpro_de$DEUPROINDMISMEI

glm(IR.STCI01DEM156N$IRSTCI01DEM156N[13:177]~0+longrun_de$IRLTLT01DEM156N[1:165]+expInf[1:165]+indpro_de$DEUPROINDMISMEI[1:165])

################
# Equation 2.4 #
################

# Here we used the expected inflation and expected output gap without do the rest 
# fourth method the variable output without the log indpro_de$DEUPROINDMISMEI
glm(IR.STCI01DEM156N$IRSTCI01DEM156N[13:177]~MA_Inflation_de+indpro_de$DEUPROINDMISMEI[1:165])

################
# Equation 2.5 #
################

r.1=IR.STCI01DEM156N$IRSTCI01DEM156N[14:177]
length(r.1)

#fourth method the variable output without the log indpro_uk$INTDSRGBM193N
glm(IR.STCI01DEM156N$IRSTCI01DEM156N[14:164]~r.1[14:164]+MA_Inflation_de[14:164]+indpro_de$DEUPROINDMISMEI[14:164])

## Lag o_t
o_t <- diff(log(DEINDPRTEX))
o_t_lag=matrix(ncol=12, nrow=177)
for (i in 1:12){
  o_t_lag[,i]=lag(o_t, i)
}
o_t_lag=as.data.frame(o_t_lag)


# Equation 2.5
length(MA_Inflation_de)
library(ivreg)
DESINTR=as.data.frame(DESINTR)

base_line_IV_DE= ivreg(DESINTR$IRSTCI01DEM156N[14:177]~r.1[1:164]+MA_Inflation_de[1:164]+expec_Xt[1:164]
                       |x_t_lag$V1[1:164]+x_t_lag$V2[1:164]
                       +x_t_lag$V3[1:164]+x_t_lag$V4[1:164]+x_t_lag$V5[1:164]
                       +x_t_lag$V6[1:164]+x_t_lag$V9[1:164] + phi_lag$V1[1:164]+
                         phi_lag$V2[1:164]+phi_lag$V3[1:164]+phi_lag$V4[1:164]+phi_lag$V5[1:164]+phi_lag$V6[1:164]+phi_lag$V9[1:164])
summary(base_line_IV_DE)


