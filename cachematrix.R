#set and get the value of the matrix and the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) 
{
	invm<- NULL
	set <- function(y) {
                x <<- y
                invm <<- NULL
        }
        get <- function() 
        x
        setinverse <- function(solve) 
        invm <<- solve()
        getinverse <- function() 
        invm
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


# Gets the inverse from the cache

cacheSolve <- function(x, ...) {
	invm <- x$getinverse()
        if(!is.null(invm)) {
                message("getting cached data")
                return(invm)
        }
        data <- x$get()
        invm <- solve(data, ...)
        x$setinverse(invm)
        invm
        
}

