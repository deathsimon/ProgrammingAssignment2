## The code computes the inverse matrix of a given NxN matrix 
## ,and keeps the result in cache.

## Usage: 
##	temp <- makeCacheMatrix(input_matrix)
##	cacheSolve(temp)

## If the result is already in cache, you'll see 
## "getting cached data" before the result.

## makeCacheMatrix() creates a data structure that keeps
## the previous matrix inversion result and four functions:
## {set, get, setInverse, getInverse}

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y){
		x <<- y
		i <<- NULL
	}
	get <- function() x
	setInverse <- function(inverse) i <<- inverse
	getInverse <- function() i
	list(set = set, get = get,
		setInverse = setInverse,
		getInverse = getInverse )
}

## cacheSolve() first checks if the inverse matrix has already 
## been cached.  If not, solve and cache it.

cacheSolve <- function(x, ...) {
 	i <- x$getInverse()
	if(!is.null(i)){
		message("getting cached data")
		return(i)
	}
	data <- x$get()
	i <- solve(data, ...)
	x$setInverse(i)
	i
}
