## runTest.R 
## runs tests and generates various tables
## Tobin Cataldo 2014

runTest <- function() {
 
    
    # attack contigency table
    mat <- matrix(c(23, 11,0,30,0,0,0,26,0,0,10,0), ncol=4,nrow=3,byrow=TRUE)
    colnames(mat) <- c("G1","G2","G3","G4")
    row.names(mat) <- c("R1","R2","R3")
    mat <- addmargins(mat)
    
    
    # dull contigency table
   #mat <- matrix(c(4,5,2,0,3,20,19,4,27,14,27,6,7,9,0,3), ncol=4, nrow=4, byrow=TRUE)
   #colnames(mat) <- c("G1","G2","G3","G4")
   #row.names(mat) <- c("R1","R2","R3","R4")
   #mat <- addmargins(mat)
    
    condProb <- conditionalProbTable(mat)
    quetTable<- queteletIndexTable(condProb)
    cumRelFreq <- cumRelativeFreqTable(mat)
    expRelFreq <- expectedFreqTable(cumRelFreq)
    pearsonchisq <- pearsonsChiSquared(cumRelFreq,expRelFreq)
    pearsonindex <- pearsonsIndex(cumRelFreq,expRelFreq)
    
    print("Contingency Table")
    print(mat)
    print("Conditional Probablity Table")
    print(condProb)
    print("Quetelet Index Table")
    print(quetTable)
    print("Cumulative Relative Frequency Table")
    print(cumRelFreq)
    print("Expected Relative Frequency Table")
    print(expRelFreq)
    print("Pearson's Chi Squared Table")
    print(pearsonchisq)
    print("Pearsons Index")
    print(pearsonindex)
}