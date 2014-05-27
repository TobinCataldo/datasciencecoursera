PrincipalComponentAnalysisComponentContribution <- function(A) {
 # take a matrix A
 # y< - svd(A) ~= [Z,Mu,C]
 # Z ~= y$u (matrix of left singular vectors of A)
 # Mu ~= y$d (vector of singular values of A)
 # C ~= y$v (matrix of right singular vectors of A)
    
 #Contribution of 1st component == Mu^2 / DataScatter
 y <- svd(A)
 vec <- (y$d^2)/dataScatter(A)
 
 #returns a vector of PCA for each column
 # need first column only?
 vec

}