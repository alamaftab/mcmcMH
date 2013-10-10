#### remove DatE from global env
myDatE <- Dt
rm(Dt)
###paramenters#########
futureNoOfDays = 20

#######################
dataCollect <- data.frame(Dt=0,predict = 0.0, realVal = 0.0)

attach(HDOrdLn)

for ( i in 1:30)
{
  
  for ( j in 21:445)
   {

    
     tmpPLn = PLn[j]
     tmpVLn = VLn[j]
     tmpPSmaLn = PSmaLn[j]
     tmpVSmaLn = VSmaLn[j]
     tmpBackPLn1 = BackPLn1[j]
     tmpDt = Dt[j]
     tmpAheadP = AheadP[j]

      
     
     for ( k in 1:futureNoOfDays)
     {
       randRow = sample(seq(1,futureNoOfDays),1)
       
       CofIncept = rnorm(1,mcmcSummary[[1]][[1]] ,mcmcSummary[[1]][[8]]/20)
       CofPLn = rnorm(1, mcmcSummary[[1]][[2]] ,mcmcSummary[[1]][[9]]/20)
       CofVLn = rnorm(1, mcmcSummary[[1]][[3]] ,mcmcSummary[[1]][[10]]/20)
       CofPSmaLn = rnorm(1, mcmcSummary[[1]][[4]] ,mcmcSummary[[1]][[11]]/20)
       CofVSmaLn = rnorm(1, mcmcSummary[[1]][[5]] ,mcmcSummary[[1]][[12]]/20)
       CofBackPLn1 = rnorm(1, mcmcSummary[[1]][[6]] ,mcmcSummary[[1]][[13]]/20)
       
       prdictVal = 0
       prdictVal = (( CofPLn*tmpPLn +    CofVLn*tmpVLn  + CofPSmaLn*tmpPSmaLn  + CofVSmaLn*tmpVSmaLn + CofBackPLn1*tmpBackPLn1 ) +  CofIncept  ) 
       
       #print(exp(prdictVal))
#      tmpBackPLn1 = tmpPLn
       tmpPLn = log( exp(prdictVal) + 0.001*exp(prdictVal)*(  - rpois(1,.02) +  rpois(1,.08)) )
       tmpPLn = prdictVal
#       tmpPLn = prdictVal 
       tmpVLn = VLn[j+ 1 - randRow]
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

plot(v, ylim = c(20,60))
lines(v1, type="o", pch=22, lty=2, col="red")
lines(aggregate(formula=v[,2]~v[,1],data=v,FUN=mean), pch=44, lty=1,lwd = 4, col="blue")


#abline(lm(dd[,2] ~ dd[,1]), lty=2)

