## Assumptions:
## 	only invertible matrices will be supplied

## makeCacheMatrix creates a vector of functions that interact
##	with the input variable x.  The functions allow to retrieve
##	the original matrix and inverse matrix, or set a new value
##	for the original matrix and inverse matrix
makeCacheMatrix <- function(x = matrix()) {

	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}  ## if a new matrix is set, clear the cached inverse matrix
	get <- function() x
	setinverse <- function(x) { inv <<- x }
	getinverse <- function() inv
	list( set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}


## cacheSolve returns the inverse of a cached matrix created by 
##	calling the makeCacheMatrix vector function.  If the inverse
##	has not been cached yet, it caches it using the set method
##	of the makeCacheMatrix vector function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getinverse()
	if(!is.null(inv)) {
		message("Getting cached data")
		return(inv)
	} ## if the value has been cached, return it and exit
	## otherwise compute the inverse and cache it
	data <- x$get()
	inv <- solve(data)
	message("Setting inverse in cache")
	x$setinverse(inv)
	inv
}
