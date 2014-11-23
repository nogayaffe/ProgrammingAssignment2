## This file contain two functions - the first is creating a "unique matrix
## the second returns the inverse matrix of the first


## This function is a builder of the "unique" matrix
## The function create the matrix and return a list of 4 object
## The object are set and get of the matrix and set and get of the inverse matrix
## The inverse matrix is NULL at the begining

makeCacheMatrix <- function(x = matrix()) {
    inverse_matrix <- NULL # initiating the inverse matrix to NULL
    set_matrix <- function(y) { # set_matrix is creating a matrix
        x <<- y
        m <<- NULL
    }
    get_matrix <- function() { # get_matrix is returning the matrix
        x
    }
    set_inverse <- function(inverse) { # set_inverse is applying the inverse matrix to inverse_matrix
        inverse_matrix <<- inverse
    }
    get_inverse <- function() { # get_inverse is returning the inverse_matrix
        inverse_matrix
    }
    list(set_matrix = set_matrix,
         get_matrix = get_matrix,
         set_inverse = set_inverse,
         get_inverse = get_inverse)
    # The function create a list with the 4 fnctions
}


## This function is returning the cached inverse matrix
## if the inverse matrix was already calculated, the function will return it from the cache
## if it wasn't it will be calculated, saved to cache and then return

cacheSolve <- function(x, ...) {
    inverse <- x$get_inverse()
    if(!is.null(inverse)) {
        return(inverse) # if the inverse matrix was already calculated and cached - return it
    }
    data <- x$get_matrix()
    inverse <- solve(data, ...) # calculating the inverse matrix
    x$set_inverse(inverse)
    inverse
    # Return a matrix that is the inverse of 'x'
}
