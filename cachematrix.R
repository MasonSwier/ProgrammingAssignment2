## Hello classmate! Thanks for reviewing my work.

## My function uses the makeVector and cacheMean functions as a template 
## to cache the inverse of a matrix. In testing, I used the following matrices
## matrix(c(1,0,5,2,1,6,3,4,0),3,3)
## matrix(c(1,0,5,2,1,6,3,4,1),3,3)
## matrix(c(1,0,2,2,0,2,1,0,0,1,0,1,1,2,1,4),4,4)

## To use the functions, you'll first need to apply the makeCacheMatrix function
## to your matrix. For example....
        ## testMatrix <- makeCacheMatrix(matrix(c(1,0,5,2,1,6,3,4,0),3,3))
## Then you will apply the cacheSolve funtion to your matrix. For example...
        ## cacheSolve(testMatrix)

makeCacheMatrix <- function(x = matrix()) {
        ## This function will allow our inverted matrix to be stored after
        ## it is calculated. 
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        invisible(list(set = set, get = get,
                setsolve = setsolve,
                getsolve = getsolve))
        ## I didn't think it was important to print the list to the console
        ## so I made it invisible.
}


## This fucntion will updated the set values and get values and store the
## inverted matrix after we calculate it for the first time.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getsolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s
}
