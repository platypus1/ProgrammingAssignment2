## this code contains 2 function that allow storing a matrix inverse in cache

## makeCacheMatrix(x), where x is a matrix, returns a list of 4 functions that can access and set the value of matrix x and its inverse
## for instance, if we assign the result to Y, then Y$get() will return x, Y$getinverse() will return the current stored value of the inverse.
makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) inv <<- inverse
	getinverse <- function() inv
	list(set = set, get = get,
	     setinverse= setinverse,
	     getinverse = getinverse)
}


## cachesolve(x) where x is a result of makeCacheMatrix function, will check if a non NULL value is already assigned to x$getinverse(), and return it, otherwise it will calculate the inverse of x$get(), and set the value of the inverse to avoid subsequent calculations.
## the inverse is stored in the function's environment

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getinverse()
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data, ...)
	x$setinverse(inv)
	inv
}
