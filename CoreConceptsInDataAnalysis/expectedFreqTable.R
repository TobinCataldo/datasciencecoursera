## expectedFreqTable.R
## generates the expected frequency table from a contingency table, actually
## from the generated cumulative relative frequency table but stacking the
## functions will be the next exercise...
## Tobin Cataldo 2014

expectedFreqTable <- function (myCumRelativeFreqTable) {
    # Takes a cumulative Relative Frequency Table and returns the
    # expected frequency table
    ## cumRelative. should have margins already
    #        n1      n2       n3     ProbTot
    # m1    (1/45)   (4/45)   (7/45)   (12/45)
    # m2    (2/45)   (5/45)   (8/45)   (15/45)
    # m3    (3/45)   (6/45)   (9/45)   (18/45)
    # Sum    s(n1)    s(n2)      s(n3)
    
    tCols <- ncol(myCumRelativeFreqTable)
    tRows <- nrow(myCumRelativeFreqTable)
    
    myMatrix <- matrix(nrow=tRows-1)
    # convert table 
    for (h in 1:(tCols-1)) {
        mat <- matrix()
        for (i in 1:(tRows-1)) {
            mat <- rbind(mat, myCumRelativeFreqTable[i,tCols] * 
                              myCumRelativeFreqTable[tRows,h])
        } 
        if (is.na(mat[1,1])) {
            mat <- as.matrix(mat[2:(nrow(mat)),1])
        }
        myMatrix <- cbind(myMatrix, mat)
    }
    if (is.na(myMatrix[1,1])) {
        myMatrix <- as.matrix(myMatrix[1:nrow(myMatrix),2:ncol(myMatrix)])
    }
    
    
    colnames(myMatrix) <- colnames(myCumRelativeFreqTable)[1:(length(colnames(myCumRelativeFreqTable))-1)]
    row.names(myMatrix) <- row.names(myCumRelativeFreqTable)[1:(length(row.names(myCumRelativeFreqTable))-1)]
    
    addmargins(myMatrix,margin=1) 
}