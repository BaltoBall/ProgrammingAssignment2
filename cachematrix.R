## The function cache the inverse of a matrix for future 
## to save system caculation

## makeCacheMatrix will store value of Matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
    ## creat bigMatrix if not yet exist
    if(!exists("bigMatrix")){
        bigMatrix<<-matrix(,nrow=0,ncol=2)
    }
    ## for every matrix passed to the function, x and it's inverse will be stored for future use.
    Matrix <-list(x,solve(x))
    bigMatrix<<-rbind(bigMatrix,Matrix)
    
}


## cacheSolve check if inverse of a function has been calculated
## the function will retur a value from cached

cacheSolve <- function(x, ...) {
    if(!exists("bigMatrix")){
        bigMatrix<<-matrix(,nrow=0,ncol=2)
    }
    ## Return a matrix that is the inverse of 'x'
    for (i in 1:nrow(bigMatrix)+1)   {
    ## if can't find stored matrix, will store the new matrix and reture inverse.    
        if (i>nrow(bigMatrix)){
            message("Caching new matrix" )
                    makeCacheMatrix(x)
                    return(bigMatrix[nrow(bigMatrix),2]$Matrix)
        }
    ## if find identical matrix in stored bigMatrix, will show inverse from cached    
        else{
            if(all(x == bigMatrix[i,1]$Matrix) ){
                message("getting cached data")
                return(bigMatrix[i,2]$Matrix)
               
                
            }
        }
    }
    
    
}
