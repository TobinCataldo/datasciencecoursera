dataScatter <- function(A) {
 #takes a matrix
 # data scatter ds = sum(sum(x.*X))
 # in R ds = sum(A*A)
 if (class(A) != "matrix") { stop("matrix required")}
 sum(A*A)

}