## makeCacheMatrix and cacheSolve are two functions that cache the values stored in a matrix
## and solve for the inverse of the matrix and then store the results. if the inverse has already
##been solved previously, then the two functions will return the cached inverse rather than 
##recalculate it again. 

##makeCacheMatrix caches the values stored in a matrix as well as the values of the matrix's
##inverse

makeCacheMatrix <- function(storedMatrix = matrix()) { 
          inverseMatrix <- NULL
          setMatrix <- function(newMatrix) { 
                    storedMatrix <<- newMatrix 
                    inverseMatrix <<- NULL                    
          }
          getMatrix <- function() storedMatrix 
          setInverse <- function(inverse) inverseMatrix <<- inverse
          getInverse <- function() inverseMatrix
          list(setMatrix = setMatrix, getMatrix = getMatrix,
               setInverse = setInverse,
               getInverse = getInverse)
}

## cacheSolve() derives the inverse of the matrix stored inside storedMatrix.
## if the matrix's inverse has already been derived, then the cacheSolve will
## call the cached inverse rather than recalculate it. 

cacheSolve <- function(storedMatrix, ...) {
        ## Return a matrix that is the inverse of 'storedMatrix'
          inverseMatrix <- storedMatrix$getInverse()
          if(!is.null(inverseMatrix)) {
                    message("getting cached data")
                    return(inverseMatrix)
          }
          data <- storedMatrix$getMatrix()
          inverseMatrix <- solve(data, ...)
          storedMatrix$setInverse(inverseMatrix)
          inverseMatrix
}