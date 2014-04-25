## Put comments here that give an overall description of what your
## functions do:
##The functions below are much like the example given in the assignment

##The fact that the solve() function returns the inverse of a matrix, allows
##us to use the original code for makevector and cachemean pratically without
##modification

## makeCacheMatrix returns a list in which each element is a function

 

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
## The "set" element stores the matrix supplied as argument and resets
## the inverse in order to prevent the cache from being returned when the
## matrix changed
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
## The "get" element returns the original matrix supplied as argument 
## to makeCacheMatrix
        get <- function() x
## The "setinverse" element stores the result of the computed inverse 
## into variable i
        setinverse <- function(inverse) i <<- inverse
## The "getinverse" element returns the cached element for the inverse
        getinverse <- function() i
## Finally the function returns each of the above functions as elements of a
## list.
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function will return the inverse of a matrix, provided that the
## matrix is of the special type defined with makeCacheMatrix
## Because the exercise specifies that we assume the matrices will be
## invertible, we make no further tests to ensure.

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
## First we try to retrive a cached value
        i <- x$getinverse()
## If the cached value is not null, we can return it with a message
## specifying that we're using cached data
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
## If the above comparison returns false, it means the inverse has not yet 
## been computed> We then retrieve the original data and calculate the inverse
        data <- x$get()
        i <- solve(data, ...)
## Last but not least we store the computed inverse so that it can be reused 
## in future calls
        x$setinverse(i)
## The last value is the inverse to be returned
        i
}

## It should be noted that calling these functions with something like
## f<-cacheSolve(makeCacheMatrix(a)) defeats the purpose of caching
## since the cached value will always be reverted to null first
## makeCacheMatrix and cacheSolve should therefore be called separately
