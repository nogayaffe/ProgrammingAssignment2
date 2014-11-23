## Put comments here that give an overall description of what your
## functions do

## This function is a builder of the "unique" matrix
## The function create the matrix and return a list of 4 object
## The object are set and get of the matrix and set and get of the inverse matrix
## The inverse matrix is NULL at the begining

makeCacheMatrix <- function(x = matrix()) {
    inverse_matrix <- NULL
    set_matrix <- function(y) {
        x <<- y
        m <<- NULL
    }
    get_matrix <- function() {
        x
    }
    set_inverse <- function(inverse) {
        inverse_matrix <<- inverse
    }
    get_inverse <- function() {
        inverse_matrix
    }
    list(set_matrix = set_matrix,
         get_matrix = get_matrix,
         set_inverse = set_inverse,
         get_inverse = get_inverse)
    
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    inverse <- x$get_inverse()
    if(!is.null(inverse)) {
        return(inverse) #if the inverse matrix was already calculated and cached - return it
    }
    data <- x$get_matrix()
    inverse <- solve(data, ...) #calculating the inverse matrix
    x$set_inverse(inverse)
    inverse
    ## Return a matrix that is the inverse of 'x'
}
