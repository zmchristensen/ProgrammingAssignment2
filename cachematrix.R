## The functions makeCacheMatrix and cacheSolve
## provide a way to cache the inverse of a matrix.

## Computing the inverse can be a relatively intense
## operation, and this reduces the overhead by saving
## the answer the first time.


## makeCacheMatrix takes a maxtrix and returns
## an object containing the matrix and 4 functions.
## These functions are getters and setters for the 
## matrix and the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL

  set <- function (y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x

  setinverse <- function(inverse) {
    i <<- inverse
  }
  getinverse <- function() {
    i
  }

  list(x,
         set = set, 
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve takes a matrix and returns the inverse.
## If the inverse has no been previously computed, the
## call to getinverse() returns null, and the inverse
## is solved for; otherwise, the cached answer is returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()

        if (!is.null(i)) {
            message("getting cached data")
            return (i)
        }

        data <- x$get()
        i <- solve(data)
        x$setinverse(i)
        i
}
