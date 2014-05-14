## contingencyTable.R
## generates a contingency table
## Tobin Cataldo 2014

contingencyTable <- function (tdata, rowsColNum, valueColNum, breaks, left=TRUE,right=FALSE){
    # data is the data set (Iris)
    # rowsColNum is the row values, 
    # valueColNum is the column of data that should be summarized
    # breaks is a vector containing the column breaks
    # left include x >= leftval (TRUE), x > leftval (FALSE)
    # right include x <= rightval (TRUE), x < rightval (FALSE)
    
    
    
    myRows <- sort(unique(tdata[,rowsColNum]))
    
    myMatrix <- matrix(ncol=(length(breaks)-1))
    
    for (myRow in myRows) {
        myMatrix <- rbind(myMatrix, 
                    table(cut(tdata[tdata[,rowsColNum]==myRow,valueColNum], 
                              breaks=breaks, include.lowest=left, right=right)))
    }
    
    if (is.na(myMatrix[1,1])) {
        myMatrix <- as.matrix(myMatrix[2:nrow(myMatrix),1:ncol(myMatrix)])
    }
    
    colnames(myMatrix) <- breaks[1:(length(breaks)-1)]
    row.names(myMatrix) <- myRows
    addmargins(myMatrix)
}