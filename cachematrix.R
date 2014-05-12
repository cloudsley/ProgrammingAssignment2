## Two functions to calculate and cache the inverse of a matrix

## Creates a list of functions that can set and get 
## the original matrix and the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
                m <- NULL
                set <- function(y) {
                        x <<- y
                        m <<- NULL
                }
                get <- function() x
                set_inv_matrix <- function(inv_matrix) m <<- inv_matrix
                get_inv_matrix <- function() m
                list(set = set, get = get,
                     set_inv_matrix = set_inv_matrix,
                     get_inv_matrix = get_inv_matrix)
        }

## Checks if the inverse matrix is already cached 
## else calculates and returns inverse matrix

cacheSolve <- function(x, ...) {
        m <- x$get_inv_matrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$set_inv_matrix(m)
        ## Return a matrix that is the inverse of 'x'
        m
}

# inv_myMatrix<-makeCacheMatrix(myMatrix)
# cacheSolve(inv_myMatrix)


