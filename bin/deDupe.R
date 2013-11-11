histDataOrd
dedupeCol = 2
dedueList <- sort(unique(histDataOrd[,dedupeCol]))
dedupeDataFrame <- histDataOrd[1,]
#put deduping logic below
itmp = dedupeDataFrame[1,2]
for ( i in dedueList)
{  if ( i != itmp )
  {
  # take first row from dupe row. or you can take last row also
    dedupeDataFrame <- rbind(dedupeDataFrame,subset(histDataOrd,histDataOrd[,2] == i )[1,])
  
  }
}