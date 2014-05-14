## queteletIndexTable.R
## generates the quetelet index table from a contingency table, actually
## from the generated conditional probability table but stacking the
## functions will be the next exercise...
## Tobin Cataldo 2014

queteletIndexTable <- function (myConProbTable) {
    ## takes a conditonal probability table and returns a table of quetelet indexes
    ## create a matrix or table where column value is value(Column)/sum(Column)
    ##
    ## contingencyTable should have margins already
    #
    #       n1  n2  n3  tot
    # m1    1   4   7    
    # m2    2   5   8
    # m3    3   6   9
    
    # conditional probability returns
    #        n1      n2       Sum
    # m1    (1/6)   (4/15)   (7/24)
    # m2    (2/6)   (5/15)   (8/24)
    # m3    (3/6)   (6/15)   (9/24)
    # tot    6       15       24
    #
    # quetelet returns
    #               y1                         y2     
    # x1    (m1,n1)-(m1,Sum)/(m1,Sum)  (m1,n2)-(m1,Sum))/(m1,Sum)
    # x2    (m1,n1)-(m1,Sum)/(m1,Sum)  (m1,n2)-(m1,Sum))/(m1,Sum)
    
    tCols <- ncol(myConProbTable)
    tRows <- nrow(myConProbTable)
    
    myMatrix <- matrix(nrow=tRows-1)
    # convert table 
    for (h in 1:(tCols-1)) {
        mat <- matrix()
        for (i in 1:(tRows-1)) {
            mat <- rbind(mat, (myConProbTable[i,h]-myConProbTable[i,tCols])/myConProbTable[i,tCols])
        } 
        if (is.na(mat[1,1])) {
            mat <- as.matrix(mat[2:(nrow(mat)),1])
        }
        myMatrix <- cbind(myMatrix, mat)
    }
    if (is.na(myMatrix[1,1])) {
        myMatrix <- as.matrix(myMatrix[1:nrow(myMatrix),2:ncol(myMatrix)])
    }
    
    colnames(myMatrix) <- colnames(myConProbTable)[1:(length(colnames(myConProbTable))-1)]
    row.names(myMatrix) <- row.names(myConProbTable)[1:(length(row.names(myConProbTable))-1)]
    myMatrix
    #addmargins(myMatrix,margin=1)
}