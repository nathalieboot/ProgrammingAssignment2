## The first function, makeCacheMatrix returns a list with information that the function
## cacheSolve needs to compute the inverse of the matrix x.
## The second function, cacheSolve, returns the inverse of matrix x using the list 
## returned by makeCacheMatrix.

## makeCacheMatrix first of all takes a(n invertible) matrix and clears any object 'm' that
## already exists by setting it to NULL. Subsequently, it defines the getters and setters 
## that the cacheSolve function needs to compute the inverse of the matrix x. makeCacheMatrix 
## returns a list that allows subsequent code to access (or change) the objects in the list.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x 
        setinv <- function(inv) m <<- inv
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## The cacheSolve function checks whether the inverse for the given matrix already exists and 
## returns it if does. If it doesn't exist, the function retrieves the matrix using the list 
## that was returned by the previous function (using x$get), stores the inverse of that matrix
## in m and returns it

cacheSolve <- function(x, ...) {
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
