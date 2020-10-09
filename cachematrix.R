## Both functions below are to store and retrieve inverse matrix, the basic idea 
## of which are almost same as the one in the example of 'mean'.

## The first function is to set or get the inverse matrix. It is not a standalone
## to figure out inverse matrix, which requires the second matrix following later.

makeCacheMatrix <- function(x=matrix()){
    i <- NULL
    set <- function(y){
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## The second function is to calculate inverse matrix. But first of all, it checks
## if there is a cache from the first function. If not, it calculate one and 
## store it in the certain part of the first function.

cacheSolve <- function(x, ...){
    i <- x$getinverse()    # to check if there is a cache
    if(!is.null(i)){       # if there is a cache
        message('get cached data')
        return(i)          # return it rather recalculate
    }       
    data <- x$get()          # if not, it calculate the inverse here
    i <- solve(data, ...)    
    x$setinverse(i)          # store it in the first fx
    i       # Return a matrix that is the inverse of 'x'
}


