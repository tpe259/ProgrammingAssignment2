# Efficient calculation of matrix inverses
# Functions to cache the calculation of matrix inverse to avoid unnecessary recalculation

# makeCacheMatrix: create list of functions to calculate inverse and cache the value
# Syntax: makeCacheMatrix(x), where x is a non-singular (invertible) square matrix
#
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL#                              Initialise variable to store matrix inverse
        set <- function(y) {
                x <<- y#                        Assign value in the parent (makeCacheMatrix) environment
                m <<- NULL#                     Clear value of m from previous running of function
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve#Calculate inverse and store in variable m
        getinverse <- function() m#             Retrieve cached value of inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)#          Create a list containing the functions defined above
}

# cacheSolve: Calculate matrix inverse, or retrieve it from cache if previously calculated
# Syntax: x_inverse <- cacheSolve(makeCacheMatrix(x)), where x is a non-singular matrix
#
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of x
        m <- x$getinverse()#                    Collect cached value from parent environment
        if(!is.null(m)) {
                message("getting cached data")# Signal when cached value is being returned
                return(m)
        }
        data <- x$get()#                        If there is no cached value, calculate the inverse
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
