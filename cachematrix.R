## Put comments here that give an overall description of what your
## functions do

## creates a special matrix that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i<- NULL                    ## define the inverse initially as null
  set<- function(y){          ## define the funcition to set the matrix data
      x<<-y
      i<<- NULL
  }
  get<- function() x          ## function to get the data of matrix
  setinv <-function(inv) i<<- inv     ## function to set the inverse of the matrix
  getinv <-function() inv             ## function to get the inverse of th matrix
    
  list(set = set, get = get,seinv = setinv, getinv = getinv)      ##returns a list

}


## WThis function computes the inverse of the special "matrix" returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
        
}
