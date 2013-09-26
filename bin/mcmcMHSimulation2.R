#### remove DatE from global env
myDatE <- Dt
rm(Dt)

dataCollect <- data.frame(Dt=0,predict = 0.0, realVal = 0.0)

attach(HDOrdLn)

for ( i in 1:10)
{
  
  for ( j in 2:150)
   {

    
     tmpPLn = PLn[j]
     tmpVLn = VLn[j]
     tmpPSmaLn = PSmaLn[j]
     tmpVSmaLn = VSmaLn[j]
     tmpBackPLn1 = BackPLn1[j]
     tmpDt = Dt[j]
     tmpAheadP = AheadP[j]

      
     
     for ( k in 1:10)
     {
       
       CofIncept = rnorm(1,mcmcSummary[[1]][[1]] ,mcmcSummary[[1]][[8]]/100)
       CofPLn = rnorm(1, mcmcSummary[[1]][[2]] ,mcmcSummary[[1]][[9]]/100)
       CofVLn = rnorm(1, mcmcSummary[[1]][[3]] ,mcmcSummary[[1]][[10]]/100)
       CofPSmaLn = rnorm(1, mcmcSummary[[1]][[4]] ,mcmcSummary[[1]][[11]]/100)
       CofVSmaLn = rnorm(1, mcmcSummary[[1]][[5]] ,mcmcSummary[[1]][[12]]/100)
       CofBackPLn1 = rnorm(1, mcmcSummary[[1]][[6]] ,mcmcSummary[[1]][[13]]/100)
       
       prdictVal = 0
       prdictVal = (( CofPLn*tmpPLn +    CofVLn*tmpVLn  + CofPSmaLn*tmpPSmaLn  + CofVSmaLn*tmpVSmaLn + CofBackPLn1*tmpBackPLn1 ) +  CofIncept ) 
       
       print(exp(prdictVal))
       tmpPLn = prdictVal
       tmpDt = Dt[j+k]
       tmpAheadP = AheadP[j+k]


       }   
     
     dataCollect <- rbind(dataCollect , c(tmpDt ,prdictVal, tmpAheadP))
   }
     
}
detach(HDOrdLn)

a <-strptime(dataCollect[,1], "%Y%m%d")
v <- data.frame(a,exp(dataCollect[,2]))
v1 <- data.frame(a,exp(dataCollect[,3]))

plot(v, ylim = c(110,140))
lines(v1, type="o", pch=22, lty=2, col="red")


#abline(lm(dd[,2] ~ dd[,1]), lty=2)

