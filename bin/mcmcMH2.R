#!/usr/bin/Rscript
library(TTR)
library(MCMCpack)



source("c:/aftab/R/mcmc/config/mcmc.cfg")
source(paste(Home, "/data/parm/mcmcMH.in", sep = ''))

#source(paste(Home, "/lib/impliedVolLib.R", sep = ''))


dataPathHist= histFile


histData <- read.table(dataPathHist,skip=0, header=F, sep="|")
histDataOrd <- histData[ order(histData$V2), ]
histDataRowCount = as.numeric(nrow(histData))
histDataOrd


SMAVolume20 <- c(0.0)
SMAPrice20 <- c(0.0)
OneDayAheadP <- c(0.0)

SMAVolume20 <- SMA(histDataOrd[,7], 20)
SMAPrice20 <- SMA(histDataOrd[,6], 20)
# get next day price array and add it to main dataframe
OneDayAheadP <- histDataOrd[(2:histDataRowCount),6]
OneDayAheadP[histDataRowCount] = 0.0

histDataOrd1 <-cbind(histDataOrd[,6], histDataOrd[,7],SMAPrice20, SMAVolume20,OneDayAheadP)
Dt <-as.integer(histDataOrd[,2])
histDataOrd1 <-cbind(Dt, histDataOrd1)

HDOrdLn <- data.frame(Dt=histDataOrd1[,1],PLn=log(histDataOrd1[,2]), VLn=log(histDataOrd1[,3]),
            PSmaLn = log(histDataOrd1[,4]) , VSmaLn =log(histDataOrd1[,5]) , AheadP = histDataOrd1[,6])


posterior1 <- MCMCregress( AheadP ~ PLn + VLn + PSmaLn + VSmaLn , data=HDOrdLn)

mcmcSummary <-summary(posterior1)

attach(HDOrdLn)

  prnew = ( mcmcSummary[[1]][[2]]*PLn +    mcmcSummary[[1]][[3]]*VLn  +  
            mcmcSummary[[1]][[4]]*PSmaLn  + mcmcSummary[[1]][[5]]*VSmaLn   ) +  mcmcSummary[[1]][[1]]  

  kk <- cbind(Dt,prnew, AheadP )


detach(HDOrdLn)

# Test the model
tt <-subset( kk,Dt < 20130910)
a <-strptime(tt[,1], "%Y%m%d")
b <- abs(tt[,2] - tt[,3])
cc <- data.frame(a,b)
plot(cc)
hist(cc[,2])




