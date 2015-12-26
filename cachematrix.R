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
