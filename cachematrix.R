## Here are two functions. First one 'makeCacheMatrix' stores the matrix with
## its inverse matrix and defines the way to use it. Second 'cacheSolve'
## returns the inverse matrix, taking it from cache if it has already been stored,
## or computing it otherwise.

## Function 'makeCacheMatrix' creates a list, containing operations over
## square marix. That allows the program to store inverse matrix in cache.

makeCacheMatrix <- function(x = matrix()) {
		i <- NULL	
        	set <- function(y) {
                x <<- y
                i <<- NULL
        	}
        	get <- function() x
        	setinverse <- function(solve) i <<- solve(x)
        	getinverse <- function() i
        	list(	set = set, get = get,
             	setinverse= setinverse,
             	getinverse = getinverse)	
}

## 'cacheSolve' function returns the inverse matrix of the 'x' argument.
## If corresponding inverse matrix for 'x' has already been computated
## function takes it value from cache  

cacheSolve <- function(x, ...) {
        
	  i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i) 

## execution of the function will stop at this line if there is cached data
    
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i	

## last line will return the computated inverse matrix
}
