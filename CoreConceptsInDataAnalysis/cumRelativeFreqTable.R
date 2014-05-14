## cumRelativeFreqTable.R
## generates the cumulative relative frequency table from a contingency table
## Tobin Cataldo 2014

cumRelativeFreqTable  <- function (myConTable) {
    ## Takes a contigency table and returns a cumulative relative frequency table
    ## create a matrix or table where column value is value(Column)/cumsum(Column)
    ##
    ## contingencyTable should have margins already
    #
    #       n1  n2  n3
    # m1    1   4   7
    # m2    2   5   8
    # m3    3   6   9
    # tot   6   15  24
    # returns
    #        n1      n2       n3     ProbTot
    # m1    (1/45)   (4/45)   (7/45)   (12/45)
    # m2    (2/45)   (5/45)   (8/45)   (15/45)
    # m3    (3/45)   (6/45)   (9/45)   (18/45)
    
        
    tCols <- ncol(myConTable)
    tRows <- nrow(myConTable)
    
    # test if table margins are added
    # do row sum == col sum
    if (sum(myConTable[tRows,1:(tCols-1)]) != sum(myConTable[1:(tRows-1),tCols])){
        
        # not equal add margins and try again
        addmargins(myConTable)
        tCols <- ncol(myConTable)
        tRows <- nrow(myConTable)
        
        # test again stop if broken
        if (sum(myConTable[tRows,1:(tCols-1)]) != sum(myConTable[1:(tRows-1),tCols])){
            stop("broken contingency table")
        }
    }
    
    myMatrix <- matrix(nrow=tRows-1)
    # convert table 
    for (h in 1:(tCols)) {
        mat <- matrix()
        for (i in 1:(tRows-1)) {
            mat <- rbind(mat, myConTable[i,h]/myConTable[tRows,tCols])
        } 
        if (is.na(mat[1,1])) {
            mat <- as.matrix(mat[2:(nrow(mat)),1])
        }
        myMatrix <- cbind(myMatrix, mat)
    }
    if (is.na(myMatrix[1,1])) {
        myMatrix <- as.matrix(myMatrix[1:nrow(myMatrix),2:ncol(myMatrix)])
    }
    
    
    colnames(myMatrix) <- colnames(myConTable)[1:(length(colnames(myConTable)))]
    row.names(myMatrix) <- row.names(myConTable)[1:(length(row.names(myConTable))-1)]
    
    addmargins(myMatrix,margin=1)
}