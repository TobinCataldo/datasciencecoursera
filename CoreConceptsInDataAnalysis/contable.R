contable <- function(species, data, breaks=c(0,0.5,1.5,2,2.6)){
    
    df <- data.frame()
   
    xx<-table(species, data)
    xy<-reshape2::melt(xx)
 
    mymatrix <- matrix(nrow=3, ncol=4)
    
    mySpecies <- sort(unique(species))
    
    for (h in 1:length(mySpecies))
    {        
        for (i in 1:4)
        { 
             xyt  <- xy[xy[1]==as.character(mySpecies[h]) & xy[2]>breaks[i] & xy[2]<=breaks[i+1],]
         
             mymatrix[h,i] <- sum(xyt$value)
        }
    }
    
    colnames(mymatrix) <- c("G1","G2","G3","G4")
    row.names(mymatrix) <- mySpecies
    addmargins(mymatrix)
}