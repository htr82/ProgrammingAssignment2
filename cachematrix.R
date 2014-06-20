## These functions are used to calculate the inverse of a matrix.
##	They ensure that the inverse is only calculated once.
##	Once the inverse is calculated, its value is put into the cache.
##	If the inverse is needed, the cache is first checked to see if it
##		has already been calculated and, if so, it is retrieved
##		from the cache.  If not, it is calculated and placed into
##		the cache

## The makeCacheMatrix function creates a vector which is a list of four functions that:
##	set the value of the vector
##	get the value of the vector
##	set the value of the inverse
##	get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {

        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)


}


## The cacheSolve function gets the inverse of a matrix.
##	If the inverse has already been calculated, it gets it from the cache
##		instead of recalculating it
##	If the inverse has not already been calculated, it calculates it and
##		puts the value in the cache using the setinverse function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m


}
