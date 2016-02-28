## In this R file, we can reduce the cost of computing the inversion of a matrix repeatedly by storing
## its inverse matrix so the next time we use it by withdrawing it from the cache matrix object we created


## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv_matrix <- NULL
    set <- function(y){
        x <<- y
        inv_matrix <<- NULL
    }
    get <- function() x
    set_inv <- function(inverse_matrix) inv_matrix <- inverse_matrix
    get_inv <- function() inv_matrix
    list(set = set, get = get, set_inv = set_inv, get_inv = get_inv)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    inv_matrix <- x$get_inv()
    if(!is.null(inv_matrix)){
        message("getting cached data")
        return(inv_matrix)
    }
    data <-x$get()
    inv_matrix <-solve(data)
    x$set_inv(inv_matrix)
    inv_matrix
    ## Return a matrix that is the inverse of 'x'
}

## I've created a testing case for this R file. It turns out that all the functions and values works properly.

test_matrix <- matrix(nrow = 3, ncol = 3,data = c(1,1,1,2,3,5,4,9,25))
y <- makeCacheMatrix(test_matrix)
class(y)
result_matrix <- cacheSolve(y)
class(result_matrix)
print(result_matrix)
