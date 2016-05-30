## These functions are for caching the inverse of a matrix
## and returning its value when it is called rather than recomputing it.

## makeCacheMatrix caches the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
	invert <- NULL
	set <- function(y) {
		x <<- y
		invert <<- NULL
	}
	get <- function() x
	setinvert <- function(inverse) invert <<- inverse 
	getinvert <- function() invert
	list(set = set,
	     get = get,
	     setinvert = setinvert,
	     getinvert = getinvert)
}


## The first half of cacheSolve returns the cached inverse of the matrix if it is available.
## If a cached inverse is unavailable, the second half solves the matrix.

cacheSolve <- function(x, ...) {
        invert <- x$getinvert()
	if (!is.null(invert)){
		message("getting cached data")
		return(invert)
	}
	mat.data <- x$getmatrix()
	invert <- solve(mat.data, ...)
	x$setinvert(invert)
	invert
}
