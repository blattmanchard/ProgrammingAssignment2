## Cache the Inverse of a Matrix
## Matrix inversion can be a costly computation, caching the inverse of a matrix
## may be more beneficial than repeatedly computing it
## The following are a pair of functions that are used to create an object that 
## stores a matrix and caches its inverse.

# This function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	inver <- NULL
	set <- function(y) {
		x <<- y
		inver <<- NULL
     }
	get <- function () x
	setInverse <- function(inverse) inver <<- inverse
	getInverse <- function() inver
	list(set = set, 
		 get = get,
		 setInverse = setInverse,
		 getInverse = getInverse)
}


# This function computes the inverse of the special matrix returned by makeCacheMatrix. 
# If the inverse has already been calculated (and the matrix has not changed), then the cacheSolve 
# should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
 	inver <- x$getInverse()
 	if(!is.null(inver)) {
 		message("getting cache data")
 		return(inver)
     }
 	data <- x$get()
 	inver <- solve(data, ...)
 	x$setInverse(inver)
 	inver
}
