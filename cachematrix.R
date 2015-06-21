## 1. Create an object to cache the inverse of a matrix
## 2. Determine the inverse of a matrix. If the inverse exists in a Cache, retrieve that.

## function to cache the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  ## Initialize to NULL
  m <- NULL
  ## Create the set method
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  ## Create the get method
  get <- function() x
  ## Method to set the inversed Matrix
  setmatrix <- function(solve) m <<- solve
  ## Method to get the inversed Matrix
  getmatrix <- function() m
  list(
    set=set,
    get=get,
    setmatrix=setmatrix,
    getmatrix=getmatrix
  )
}


## function to return the inverse of a matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
 ## get current value
  m <- x$getmatrix()
  ## return if not null
  if(!is.null(m)){
    message("Returned from Cache")
    return(m)
  }
 ## Else calculate inverse and set value to cache
  matrix.data <- x$get()
  inverse <- solve(matrix.data,...)
  x$setmatrix(inverse)
  ## Return inverse
  return(inverse)
}

## Lines below for testing - Ignore
## testmat1 <- matrix(5:8,2,2)
## tempmat <- makeCacheMatrix(testmat1)
## soln <- cacheSolve(tempmat)
## soln
## soln <- cacheSolve(tempmat)
## soln