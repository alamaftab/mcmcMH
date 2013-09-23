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


relativePrice <- c(0.0)
relativeVolume <- c(0.0)
SMAMomentum <- c(0.0)

SMAMomentum <- (histDataOrd[,6]*histDataOrd[,7])
SMAVolume20 <- c(0.0)
SMAPrice20 <- c(0.0)
SMAMomentum20 <- c(0.0)

SMAVolume20 <- SMA(histDataOrd[,7], 20)
SMAPrice20 <- SMA(histDataOrd[,6], 20)
SMAMomentum20 <- SMA(SMAMomentum,20)

histDataOrd1 <-cbind(histDataOrd[,6], histDataOrd[,7],SMAMomentum,SMAPrice20, SMAVolume20,SMAMomentum20)
DatE <-as.integer(histDataOrd[,2])
## agument histdataord1 with date
histDataOrd1 <-cbind(DatE, histDataOrd1)
relativePriceDaily <- c(0)
relativePriceSMA20 <- c(0)
relativeVolumeSMA20 <-c(0)
relativeMomentumSMA20 <-c(0)


for (i in 2:(histDataRowCount))
{ 
 
  relativePriceDaily[i] <- (round(((histDataOrd1[(i),2] - histDataOrd1[(i-1),2])/histDataOrd1[(i-1),2])*10000,digits = 2))
  relativePriceSMA20[i] <- (round(((histDataOrd1[i,2] - histDataOrd1[i,5])/histDataOrd1[i,5])*10000,digits = 2))
  relativeVolumeSMA20[i] <- (round(((histDataOrd1[i,3] - histDataOrd1[i,6])/histDataOrd1[i,6])*10000, digits = 2))
  relativeMomentumSMA20[i] <- (round(((histDataOrd1[i,4] - histDataOrd1[i,7])/histDataOrd1[i,7])*10000, digits = 2))
}

relativeVolumeSMA20 <- sign(relativeVolumeSMA20)*log(abs(relativeVolumeSMA20))
relativeMomentumSMA20 <- sign( relativeMomentumSMA20)*log(abs( relativeMomentumSMA20))


relative.data <- cbind(histDataOrd1[,1], relativePriceDaily, relativePriceSMA20,relativeVolumeSMA20 ,relativeMomentumSMA20  )
relative.data.denorm <- data.frame(DatE = NA,
                                   prRel_1= NA,prRelSMA20_1= NA,volRelSMA20_1= NA, momentumRelSMA20_1= NA, 
                                   prRel= NA,prRelSMA20= NA,volRelSMA20= NA, momentumRelSMA20= NA, 
                        relativePriceDailyCur = NA)

for (i in 1:(histDataRowCount))
{ 
  if (i < 3) {
    tempRow <- c(relative.data[i,1], NA,NA,NA,NA,NA,NA,NA,NA, relative.data[i,2])
  }
  else {
    tempRow <- c(relative.data[i,1],
                 relative.data[(i-2),2],relative.data[(i-2),3],relative.data[(i-2),4],relative.data[(i-2),5],
                 relative.data[(i-1),2],relative.data[(i-1),3],relative.data[(i-1),4],relative.data[(i-1),5],
                 relative.data[i,2] )
  }
  
  relative.data.denorm <- rbind(relative.data.denorm, tempRow)
}

relative.data.denorm1 <-relative.data.denorm[2:(histDataRowCount + 1),]


posterior1 <- MCMCregress( relativePriceDailyCur ~ prRel + prRelSMA20 +  momentumRelSMA20 +
                           prRel_1 + prRelSMA20_1 +  momentumRelSMA20_1
                           , data=relative.data.denorm1)

mcmcSummary <-summary(posterior1)
# how to access values in summary objects.
#mcmcSummary[[1]][[10]]

attach(relative.data.denorm1)
#prnew = (1/100)*( -62.89*prRel + 73.85*prRelSMA20  + 4.584*volRelSMA20 
#                  -5.404*momentumRelSMA20 + 11.97*prRel_1 -58.55*prRelSMA20_1 +
#                    3.423*volRelSMA20_1 -2.862*momentumRelSMA20_1) + 5.375
prnew = ( mcmcSummary[[1]][[2]]*prRel +    mcmcSummary[[1]][[3]]*prRelSMA20  +  
            mcmcSummary[[1]][[4]]*momentumRelSMA20  + mcmcSummary[[1]][[5]]*prRel_1  +
          mcmcSummary[[1]][[6]]*prRelSMA20_1  + mcmcSummary[[1]][[7]]*momentumRelSMA20_1 ) +  mcmcSummary[[1]][[1]]  

kk <- cbind(DatE,prnew, relative.data.denorm1$relativePriceDailyCur)


detach(relative.data.denorm1)

tt <-subset( kk,DatE > 20130101)
a <-strptime(tt[,1], "%Y%m%d")
b <- abs(tt[,2] - tt[,3])
cc <- data.frame(a,b)
plot(cc)
hist(cc[,2])





