## For this assignment we are tring to optimize working on 
## big amounts of data that need to be analyzed repeatedly.

## This first function gets the input 
## and can create an inverse matrix and cache it.

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y){
		x <<- y
		m <<- NULL
	}
	get <- function()x
	setInverse <- function(inverse) m <<- inverse
	getInverse <- function() m 
	list(set = set, get = get, 
		setInverse = setInverse, 
		getInverse = getInverse)
}

## This second function is meant to return the value 
## for the inverted matrix, but it can first check 
## if the values are cached and use the existing version for output.

cacheSolve <- function(x, ...) {
	m <- x$getInverse()
	if(!is.null(m)){
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data,...)
	x$setInverse(m)
	m
}

##Now we might check if the functions work.

m <- matrix(rnorm(4),2,2)
m1 <- makeCacheMatrix(m)
