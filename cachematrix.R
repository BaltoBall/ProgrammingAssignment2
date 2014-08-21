## Caching the Inverse of a Matrix in order to save computation.

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  m<<- x
  inv<<-solve(x)
}

## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
cacheSolve<- function(x, ...){
  if(all(x == m) ){
    message("getting cached data")
    return(inv)
  }
  else{
    message("Caching new matrix" )
    makeCacheMatrix(x)
    cachesolve(x)
  }
}
