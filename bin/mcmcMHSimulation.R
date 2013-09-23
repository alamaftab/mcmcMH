#va <- c(0.0)
#### remove DatE from global env
myDatE <- DatE
rm(DatE)
#tmpDenorm <- data.frame(DatE = NA,  prRel_1= NA,prRelSMA20_1= NA,volRelSMA20_1= NA, momentumRelSMA20_1= NA, 
#                                  prRel= NA,prRelSMA20= NA,volRelSMA20= NA, momentumRelSMA20= NA, 
#                                   relativePriceDailyCur = NA)
dataCollect <- data.frame(DatE=0,prCalculated = 0.0)

for ( i in 1:40)
{

  
  InterceptConf = rnorm(1, mcmcSummary[[1]][[1]], mcmcSummary[[1]][[9]])
  prRelConf = rnorm(1, mcmcSummary[[1]][[2]], mcmcSummary[[1]][[10]])
  prRelSMA20Conf = rnorm(1, mcmcSummary[[1]][[3]], mcmcSummary[[1]][[11]])
  momentumRelSMA20Conf = rnorm(1, mcmcSummary[[1]][[4]], mcmcSummary[[1]][[12]])
  prRel_1Conf = rnorm(1, mcmcSummary[[1]][[5]], mcmcSummary[[1]][[13]])
  prRelSMA20_1Conf = rnorm(1, mcmcSummary[[1]][[6]], mcmcSummary[[1]][[14]]) 
  momentumRelSMA20_1Conf = rnorm(1, mcmcSummary[[1]][[7]], mcmcSummary[[1]][[15]]) 
  
  merge.relative.data.denorm1 <-merge(relative.data.denorm1,histDataOrd1,by.relative.data.denorm1 = DatE, by.histDataOrd1 = DatE)
  
   for ( j in 40:100)
   {
     tmpCurval<- c(0.0)
     tmpCurvalDollar = c(0.0)
     tmpCurval[1]= prRelConf*merge.relative.data.denorm1$prRel[j]
     tmpCurvalDollar[1]= prRelConf*merge.relative.data.denorm1$V2[j]
                     
     attach(merge.relative.data.denorm1)
     
     for ( k in 2:10)
     {

       tmpCurval[k]= ( prRelConf*tmpCurval[k-1] +   prRelSMA20Conf*prRelSMA20[j]  +  momentumRelSMA20Conf*momentumRelSMA20[j] + 
                   + prRel_1Conf*prRel_1[j] + prRelSMA20_1Conf*prRelSMA20_1[j] + 
                       momentumRelSMA20_1Conf*momentumRelSMA20_1[j] )  +  InterceptConf
      
       tmpCurvalDollar[k] = (tmpCurval[k]*V2[k-1]/10000) + V2[k-1]
      #copy tmpcurval to tmpdenorm[k] and get other value from tmpprRel_1 which is just an average. Not done yet
     }   
     detach(merge.relative.data.denorm1)
     
     dataCollect <- rbind(dataCollect , c(merge.relative.data.denorm1$DatE[j+10],tmpCurvalDollar[10]))
   }
     
}
dataCollect1 <- dataCollect[2:as.numeric(nrow(dataCollect)),]
a <-strptime(dataCollect1[,1], "%Y%m%d")
v <- data.frame(a,dataCollect1[,2])
#hist(va)#
dd <- data.frame(strptime(relative.data.denorm1$DatE[40:100], "%Y%m%d"),relative.data.denorm1$relativePriceDailyCur[40:100])
plot(v)
abline(lm(dd[,2] ~ dd[,1]), lty=2)

