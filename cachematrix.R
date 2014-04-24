 ## This is a pair of functions that cache the inverse of a matrix.  
 ## Assumption is that the matrix supplied is always invertible.

## makeCacheMatrix function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
 ## takes an argument x of type matrix
 
inv_x <- NULL
  set <- function(y) {
    x <<- y
    inv_x <<- NULL
  }
  get <- function() x
  setinverse<- function(inverse) inv_x <<-inverse
  getinverse <- function() inv_x
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv_x <- x$getinverse()
  if (!is.null(inv_x)) {
    message("getting cached inverse matrix")
    return(inv_x)  #just return the cache, no computation needed

  } else {
    inv_x <- solve(x$get()) #Inverse the matrix using solve function if there's no cache

    x$setinverse(inv_x) #save the result back to x's cache
    return(inv_x) #return the cache
  }
}
