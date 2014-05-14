## pearsonsChiSquared.R
## generates Pearson's Chi Squared table from a contingency table, actually
## from the generated cumulative relative frequency table and 
## the expected relative frequency table, but stacking the
## functions will be the next exercise...
## Tobin Cataldo 2014

pearsonsChiSquared <- function (myCumRelFreqTable, myExpFreqTable) {
    ##
    ## =((I27-I41)^2)/I41 
    ## (CumRel-ExpRel)^2 / ExpRel
    ## include summary quetelet index (rowsum)
    
    ## Difference between this and pearsonsIndex
    ##  pearsonsIndex                 This 
    ## (I27-I41)/SQRT(I41)            ((I27-I41)^2)/I41 
    ## (CumRel-ExpRel)/SQRT(ExpRel)   (CumRel-ExpRel)^2 / ExpRel
    
    tColsCumRel <- ncol(myCumRelFreqTable)
    if (colnames(myCumRelFreqTable)[tColsCumRel]=="Sum") {
        tColsCumRel <- tColsCumRel - 1 # drop margin
    } 
    tRowsCumRel <- nrow(myCumRelFreqTable)
    if (row.names(myCumRelFreqTable)[tRowsCumRel]=="Sum") {
        tRowsCumRel <- tRowsCumRel - 1 # drop margin
    }
    
    tColsExpFreq <- ncol(myExpFreqTable)
    if (colnames(myExpFreqTable)[tColsExpFreq]=="Sum") {
        tColsExpFreq <- tColsExpFreq - 1 # drop margin
    } 
    tRowsExpFreq <- nrow(myExpFreqTable)
    if (row.names(myExpFreqTable)[tRowsExpFreq]=="Sum") {
        tRowsExpFreq <-  tRowsExpFreq - 1 # drop margin
    }
    
    if (tColsCumRel != tColsExpFreq | tRowsCumRel != tRowsExpFreq) {
        stop("tables don't align")
    }
    
    myMatrix <- matrix(nrow=tRowsCumRel)
    # convert table 
    for (h in 1:(tColsCumRel)) {
        mat <- matrix()
        for (i in 1:(tRowsCumRel)) {
            #(CumRel-ExpRel)^2 / ExpRel 
            # myCumRelFreqTable, myExpFreqTable
            mat <- rbind(mat, ((myCumRelFreqTable[i,h] - myExpFreqTable[i,h])^2) / myExpFreqTable[i,h])
        } 
        if (is.na(mat[1,1])) {
            mat <- as.matrix(mat[2:(nrow(mat)),1])
        }
        myMatrix <- cbind(myMatrix, mat)
    }
    if (is.na(myMatrix[1,1])) {
        myMatrix <- as.matrix(myMatrix[1:nrow(myMatrix),2:ncol(myMatrix)])
    }
    
    
    colnames(myMatrix) <- colnames(myCumRelFreqTable)[1:tColsCumRel]
    row.names(myMatrix) <- row.names(myCumRelFreqTable)[1:tRowsCumRel]
    
    addmargins(myMatrix)
}