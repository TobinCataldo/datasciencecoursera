PrincipalComponentAnalysisComponents <- function(A) {
 # take a matrix A
 # y< - svd(A) ~= [Z,Mu,C]
 # Z ~= y$u (matrix of left singular vectors of A)
 # Mu ~= y$d (vector of singular values of A)
 # C ~= y$v (matrix of right singular vectors of A)
    
 #Mark(Stud,Sub)= TalentScore(Stud) * Loading(Sub)
 # Mark(stud,Sub) = (sqrt(Mu[1) * Z[,1]) * (sqrt(Mu[1] * C[1])
 y <- svd(A)
 # need absolue values
 Z <- abs(y$u)
 Mu <- abs(y$d)
 C <- abs(y$v)
 
 # HiddenScore
 #hs <- sqrt(Mu[1]) * Z[,1]
 
 # Loadings
 loadings<-sqrt(Mu[1]) * C[,1] 
 
 #as.matrix(hs) %*% loadings
}