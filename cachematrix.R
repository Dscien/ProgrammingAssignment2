## call via cacheSolve(makeCacheMatrix(matrix))

## creates a listmatrix

makeCacheMatrix <- function(m = matrix()) {
  i <- NULL
  set <- function(newmatrix) {
    m <<- newmatrix
    i <<- NULL
  }
  get <- function() m
  setinverse <- function(inv) i <<- inv
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## inverts a listmatrix

cacheSolve <- function(m, ...) {
        ## Return a matrix i that is the inverse of 'm'
  i <- m$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- m$get()
  i <- solve(data)
  m$setinverse(i)
  i
}