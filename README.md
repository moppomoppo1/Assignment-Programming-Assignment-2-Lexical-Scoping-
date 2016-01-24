# Assignment-Programming-Assignment-2-Lexical-Scoping-
makeVector <- function(x = numeric()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmean <- function(mean) m <<- mean
        getmean <- function() m
        list(set = set, get = get,
             setmean = setmean,
             getmean = getmean)
}
cachemean <- function(x, ...) {
        m <- x$getmean()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- mean(data, ...)
        x$setmean(m)
        m

#my solution 
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setInverse(inv)
        inv
}

#test
> my_matrix$set(matrix(c(5, 8, 9, 3), 2, 2))
> my_matrix$get()
     [,1] [,2]
[1,]    5    9
[2,]    8    3
> my_matrix$getInverse()
NULL
> cacheSolve(my_matrix)
            [,1]       [,2]
[1,] -0.05263158  0.1578947
[2,]  0.14035088 -0.0877193
> my_matrix$getInverse()
            [,1]       [,2]
[1,] -0.05263158  0.1578947
[2,]  0.14035088 -0.0877193
