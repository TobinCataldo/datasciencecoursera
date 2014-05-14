## conditionalProbTable.R
## generates the conditional probability table from a contingency table
## Tobin Cataldo 2014

conditionalProbTable <- function (myConTable) {
    ## takes a contigency table and returns a conditional probability table
    ## create a matrix or table where column value is value(Column)/sum(Column)
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
    # m1    (1/6)   (4/15)   (7/24)   (12/45)
    # m2    (2/6)   (5/15)   (8/24)   (15/45)
    # m3    (3/6)   (6/15)   (9/24)   (18/45)
    # tot    6       15       24      45
    #
    
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
            mat <- rbind(mat, myConTable[i,h]/myConTable[tRows,h])
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