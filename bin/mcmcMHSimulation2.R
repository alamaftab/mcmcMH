#### remove DatE from global env
myDatE <- Dt
rm(Dt)

dataCollect <- data.frame(Dt=0,predict = 0.0, realVal = 0.0)

attach(HDOrdLn)

for ( i in 1:30)
{
  
  for ( j in 100:221)
   {

    
     tmpPLn = PLn[j]
     tmpVLn = VLn[j]
     tmpPSmaLn = PSmaLn[j]
     tmpVSmaLn = VSmaLn[j]
     tmpBackPLn1 = BackPLn1[j]
     tmpDt = Dt[j]
     tmpAheadP = AheadP[j]

      
     
     for ( k in 1:20)
     {
       
       CofIncept = rnorm(1,mcmcSummary[[1]][[1]] ,mcmcSummary[[1]][[8]]/20)
       CofPLn = rnorm(1, mcmcSummary[[1]][[2]] ,mcmcSummary[[1]][[9]]/20)
       CofVLn = rnorm(1, mcmcSummary[[1]][[3]] ,mcmcSummary[[1]][[10]]/20)
       CofPSmaLn = rnorm(1, mcmcSummary[[1]][[4]] ,mcmcSummary[[1]][[11]]/20)
       CofVSmaLn = rnorm(1, mcmcSummary[[1]][[5]] ,mcmcSummary[[1]][[12]]/20)
       CofBackPLn1 = rnorm(1, mcmcSummary[[1]][[6]] ,mcmcSummary[[1]][[13]]/20)
       
       prdictVal = 0
       prdictVal = (( CofPLn*tmpPLn +    CofVLn*tmpVLn  + CofPSmaLn*tmpPSmaLn  + CofVSmaLn*tmpVSmaLn + CofBackPLn1*tmpBackPLn1 ) +  CofIncept  ) 
       
       #print(exp(prdictVal))
       CofBackPLn1 = tmpPLn
       tmpPLn = log( exp(prdictVal) + 0.20*exp(prdictVal)*(  - rpois(1,.01) +  rpois(1,.08)) )
#       tmpPLn = prdictVal 
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

plot(v, ylim = c(5,12))
lines(v1, type="o", pch=22, lty=2, col="red")
lines(aggregate(formula=v[,2]~v[,1],data=v,FUN=mean), pch=44, lty=26, col="green")
#lines(aggregate(formula=v[,2]~v[,1],data=v,FUN="green"))
#lines(aggregate(formula=v[,2]~v[,1],data=v,FUN="blue"))


#abline(lm(dd[,2] ~ dd[,1]), lty=2)

