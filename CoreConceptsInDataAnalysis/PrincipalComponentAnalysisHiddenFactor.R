PrincipalComponentAnalysisHiddenFactor <- function(A, toScale=T) {
    # take a matrix A
    # y< - svd(A) ~= [Z,Mu,C]
    # Z ~= y$u (matrix of left singular vectors of A)
    # Mu ~= y$d (vector of singular values of A)
    # C ~= y$v (matrix of right singular vectors of A)
    
    
    y <- svd(A)
    # need absolue values
    Z <- abs(y$u)
    Mu <- abs(y$d)
    C <- abs(y$v)    
    
    alpha = 1/sum(C[,1])
    
    factorLoads <- C[,1] * alpha
    
    factored <- t(t(A) * factorLoads)
    
    if (toScale) {
        library(scales)
        hiddenfactorscaled <- rescale(rowSums(factored),c(0,100))
    }
    else {
        rowSums(factored)
        #loadings<-sqrt(Mu[1]) * C[,1] 
    }
    loadings<-sqrt(Mu[1]) * C[,1] 
 
}