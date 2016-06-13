## This programming project requires writing an R function is able to cache potentially time-consuming 
## computations. For example, taking the inverse of a matrix is usually a costly computation and 
## there may be some benefit to caching an inverse of a matrix rather than compute it repeatedly.
## In this work, I will take advantage of the scoping rules of the R language and how they can be
## manipulated to preserve state inside of an R object.

## The first function makeCacheMatrix creats a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
             m <- NULL
             set <- function(y){
                        x <<-y
                        m <<-NULL
             }
             get <-function() x
             setinverse <- function(inverse) m <<- inverse
             getinverse <-function () m 
             list(set =set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The second function cacheSolve computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed),
## then the chachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
       if(!is.null(m)){
              message("getting cached data")
              return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
