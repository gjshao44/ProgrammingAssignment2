## makeCachedMatrix creates a list that can store the original matrix and cached inverse
## cacheSolve utilizes the list created by makeCacheMatrix to return the inverse and only 
## calculate the inverse if cached inverse is not found

## makeCacheMatrix: make a cached matrix with get/set/getinverse/setinverse methods

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	get <- function() x
	setinverse <- function(inv) i <<- inv
	getinverse <- function() i
	list(set = set, get = get,
	   setinverse = setinverse,
	   getinverse = getinverse)
}


## cacheSolve: solve a matrix for its inverse. Retrieve cached inverse if already solved, otherwise solve and 
## store the cache

cacheSolve <- function(x, ...) {

	i <- x$getinverse()
	if(!is.null(i)) {
		message("getting cached inverse matrix")
		return(i)
	}
	data <- x$get()
	i <- solve(data, ...)
	x$setinverse(i)
	## Return a matrix that is the inverse of 'x'
	i
}
