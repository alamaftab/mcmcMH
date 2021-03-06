## following function can be used for deduping duplicate rows by given column.

dedupe.takeFirst <-function(dsIn, Ordercolnum)
  {
    histDataOrd = dsIn
    dedupeCol = Ordercolnum
    dedueList <- sort(unique(histDataOrd[,dedupeCol]))
    dedupeDataFrame <- histDataOrd[1,]
    #put deduping logic below
    itmp = dedupeDataFrame[1,2]
    for ( i in dedueList)
    {  
      if ( i != itmp )
      {
        # take first row from dupe row. or you can take last row also
        dedupeDataFrame <- rbind(dedupeDataFrame,subset(histDataOrd,histDataOrd[,2] == i )[1,])
  
      }
    }
    return(dedupeDataFrame)
  }


## S: value of asset, K: Strike price T: tresury rate  r: time in years ( days/252) sig: hist vol 

## Black-Scholes Function
BS <-function(S, K, T, r, sig, type="C"){
  d1 <- (log(S/K) + (r + sig^2/2)*T) / (sig*sqrt(T))
  d2 <- d1 - sig*sqrt(T)
  if(type=="C"){
    value <- S*pnorm(d1) - K*exp(-r*T)*pnorm(d2)
  }
  if(type=="P"){
    value <- K*exp(-r*T)*pnorm(-d2) - S*pnorm(-d1)
  }
  return(value)
}


## S: value of asset, K: Strike price T: tresury rate  r: time in years ( days/252) market: option price in the market
## Function to find BS Implied Vol using Bisection Method
implied.vol <-function(S, K, T, r, market, type){
  sig <- 10
  sig.up <- 40
  sig.down <- 0
  count <- 0
  err <- BS(S, K, T, r, sig, type) - market 
 
  ## repeat until error is sufficiently small or counter hits 1000
  while(abs(err) > 0.01 && count<10000){
    if(err < 0){
      sig.down <- sig
      sig <- (sig.up + sig)/2
    }else{	
      sig.up <- sig
      sig <- (sig.down + sig)/2
    }
    err <- BS(S, K, T, r, sig, type) - market
    count <- count + 1
  }
 
  ## return NA if counter hit 1000
  if(count==1000){
    return(NA)
  }else{
    return(sig)
  }
}
#Test the library:
#BS(42,55,.012,29/252,2.08, "C")
#implied.vol(42,55,.012,(29/252),.65, "C")

