rm(list = ls())

graphics.off()

options(digits=4)

#Industrial Production for DE
DEUPROINDMISMEI <- getSymbols("DEUPROINDMISMEI", src = "FRED", from = "1979-01-01", auto.assign = FALSE)
DE.INDPRO <- window(DEUPROINDMISMEI, start ="1979-04-01",end = "1993-12-31")
head(DE.INDPRO)
tail(DE.INDPRO)

#Industrial production index for DE
A018ADDEA338NNBR <- getSymbols("A018ADDEA338NNBR", src = "FRED", from = "1979-01-01", auto.assign = FALSE)
DEINDEX.INDPRO <- window(A018ADDEA338NNBR, start ="1979-04-01",end = "1993-12-31")
head(DEINDEX.INDPRO)
tail(DEINDEX.INDPRO)


#Consumer Price Inflation for DE
CPA.LTT01DEM659N <- getSymbols("CPALTT01DEM659N", src='FRED',from ="1979-01-01", auto.assign = FALSE)
DECPA <- window(CPA.LTT01DEM659N,start="1979-04-01",end = "1993-12-31")
head(DECPA)
tail(DECPA)

#For the graph
CPALTT01DEM659N <- getSymbols("CPALTT01DEM659N", src='FRED',from ="1973-01-01", auto.assign = FALSE)
DE.CPA <- window(CPALTT01DEM659N,start="1973-01-01",end = "1993-12-31")
head(DE.CPA)
tail(DE.CPA)

#Short term interest rate for DE
IR.3TIB01DEM156N <- getSymbols("IR3TIB01DEM156N", src='FRED',from ="1979-01-01", auto.assign = FALSE)
DESINTR <- window(IR.3TIB01DEM156N,start="1979-04-01",end = "1993-12-31")
head(DESINTR)
tail(DESINTR)

#For the graph
IR3TIB01DEM156N <- getSymbols("IR3TIB01DEM156N", src='FRED',from ="1973-01-01", auto.assign = FALSE)
DE.SINTR <- window(IR3TIB01DEM156N,start="1973-01-01",end = "1993-12-31")
head(DE.SINTR)
tail(DE.SINTR)

#Long term interest rate for DE
IRLTLT01DEM156N <- getSymbols("IRLTLT01DEM156N", src='FRED',from ="1974-01-01", auto.assign = FALSE)
DE.LINTR <- window(IRLTLT01DEM156N,start="1979-04-01",end = "1993-12-31")
head(DE.LINTR)
tail(DE.LINTR)

#Graph of DE
de.cpi <- data.frame(date=index(DE.CPA), coredata(DE.CPA))
de.sintr <- data.frame(date=index(DE.SINTR), coredata(DE.SINTR))
de.data <- merge(de.cpi, de.sintr)
date.de <- sort(de.data$date)
date.de[73]

library(ggplot2)

de.graph <- ggplot(de.data, aes(x = date)) + 
  geom_line(aes(y = CPALTT01DEM659N ), color="steelblue", linetype="twodash") + 
  geom_line(aes(y = IR3TIB01DEM156N), color="darkred")

de.graph + geom_vline(xintercept = date.de[73], color = "red")




