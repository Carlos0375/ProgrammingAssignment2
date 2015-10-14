## Write a short comment describing this function

## The function makeCacheMatrix stores the functions get, set, setsolve and
## getsolve. As an argument it accepts a matrix. 
## To use this stored functions the main function needs to be subset.

makeCacheMatrix <- function(x = matrix()) {
        im <- NULL
        set <- function(y) {       # Changes the matrix stored in the main
                x <<- y            # function! This is done by the operator <<-
                im <<- NULL
        }
        get <- function() x         # No arguments required!
                                    # Retruns the matrix from the main function.
        
        setsolve <- function(solve) im <<- solve # 
        getsolve <- function() im
        list(set = set, get = get,  # To store the 4 functions in the main
             setsolve = setsolve,   # main function!  
             getsolve = getsolve)
}


## Write a short comment describing this function

## The function cacheSolve first looks up if the inverse Matrix has been
## already calculated. If so no new computation is performed and the calculated
## inverse matrix is returned.
## If the inverse matrix has not yet been calculated the function cacheSolve
## calculates the inverse matrix and passes the data to the function
## makeCacheMatrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        im <- x$getsolve()          # If the inverse matrix has already been
        if(!is.null(im)) {          # calculated it is returned (im is not NULL)
                message("getting cached data")
                return(im)
        }
        data <- x$get()             # Get the matrix of the main function.
        im <- solve(data, ...)      # Calculate the inverse matrix.
        x$setsolve(im)              # Set the value of the inverse matrix.
        im                          # Display the the inverse matrix
}

## Example - run the scripts

a <- makeCacheMatrix(matrix(1:4,2,2))
a$get()

b <- cacheSolve(a)
a$get()%*%b 
