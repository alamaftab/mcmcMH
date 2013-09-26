#### remove DatE from global env
myDatE <- Dt
rm(Dt)

dataCollect <- data.frame(Dt=0,predict = 0.0, realVal = 0.0)

attach(HDOrdLn)

for ( i in 1:10)
{
    CofIncept = rnorm(1,mcmcSummary[[1]][[1]] ,mcmcSummary[[1]][[7]]/1000)
    CofPLn = rnorm(1, mcmcSummary[[1]][[2]] ,mcmcSummary[[1]][[8]]/100)
    CofVLn = rnorm(1, mcmcSummary[[1]][[3]] ,mcmcSummary[[1]][[9]]/100)
    CofPSmaLn = rnorm(1, mcmcSummary[[1]][[4]] ,mcmcSummary[[1]][[10]]/100)
    #CofPSmaLn = mcmcSummary[[1]][[4]] 
    CofVSmaLn = rnorm(1, mcmcSummary[[1]][[5]] ,mcmcSummary[[1]][[11]]/100)

 
  
  for ( j in 50:60)
   {
     tmpPLn = PLn[j]
     tmpVLn = VLn[j]
     tmpPSmaLn = PSmaLn[j]
     tmpVSmaLn = VSmaLn[j]
     tmpDt = Dt[j]
     tmpAheadP = AheadP[j]

      
     
     for ( k in 1:10)
     {
       prdictVal = 0
       prdictVal = ( CofPLn*tmpPLn +    CofVLn*tmpVLn  + CofPSmaLn*tmpPSmaLn  + CofVSmaLn*tmpVSmaLn ) +  CofIncept  
       
       print(prdictVal)
       tmpPLn = log(prdictVal)
       tmpDt = Dt[j+k]
       tmpAheadP = AheadP[j+k]


       }   
     
     dataCollect <- rbind(dataCollect , c(tmpDt ,prdictVal, tmpAheadP))
   }
     
}
detach(HDOrdLn)

a <-strptime(dataCollect[,1], "%Y%m%d")
v <- data.frame(a,dataCollect[,2])

plot(v)

#abline(lm(dd[,2] ~ dd[,1]), lty=2)

