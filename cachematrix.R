# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to cache the inverse of a matrix.

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix


makecachematrix <- function(x = matrix()) {
        minv <- NULL
        set <- function(y) {
                x <<- y
                minv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) minv <<- inverse
        getinverse <- function() minv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


# The following function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function.

cacheSolve <- function(x, ...) {
        minv <- x$getinverse()
        if(!is.null(minv)) {
                message("getting cached data.")
                return(minv)
        }
        data <- x$get()
        minv <- solve(data)
        x$setinverse(minv)
        minv
}

##Test Code
x <- rbind(c(4, 3), c(3, 2))
x
y <- makecachematrix(x)
y$get()
cacheSolve(y)
cacheSolve(y)
