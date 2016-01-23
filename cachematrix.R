
## creates new "matrix" with ability to cache it's inversion

makeCacheMatrix <- function(x = matrix()) {
  inversed <- NULL
  set <- function(y) {
    x <<- y
    inversed <<- NULL
  }
  
  get <- function() x
  
  setInversed <- function(inversedMatrix) {
    inversed <<- inversedMatrix
  }
  
  getInversed <- function() inversed
  
  list(set = set, get = get, setInversed = setInversed, getInversed = getInversed)
  

}


## computes inversed matrix if it isnt' already cached 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inversed <- x$getInversed()
    if(!is.null(inversed)) {
      message("inversed is already cached, getting data")
      return(inversed)
    }
    matrix <- x$get()
    inversed <- solve(matrix)
    x$setInversed(inversed)
    inversed
}
