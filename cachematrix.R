## The function "makeCacheMatrix"  will generate a "special" matrix object 
## that is able to cache its own inverse.
## The functions it uses are to set, get the value of the matrix and to set and get the 
## inverse of this matrix

makeCacheMatrix <- function(x = matrix()) 
        {
                ## initializing variable by setting it to NULL
                m <- NULL
                ## set the inverse of the matrix
                setmatrix <- function(matrix) m <<- matrix
                ## get the inverse of the matrix
                getmatrix <- function() m
                ## get the value of the matrix
                get <- function() x
                ## set the value of the matrix
                set <- function(y) 
                        {
                        x <<- y
                        m <<- NULL
                        }
                ## return a list with functions
                list(setmatrix = setmatrix,
                     getmatrix = getmatrix,
                     get = get, 
                     set = set)                           
        }

## The function "cacheSolve" calculates the inverse of the
## "special" matrix. The first step is to check if the matrix is already calculated.
## If so it gets the value of the inverse matrix and uses it for output.
## Otherwise the inverse is first calculated and secondly stored in cache.

cacheSolve <- function(x, ...) 
        {
                m <- x$getmatrix()
                ## checks if inverse is already calculated
                if(!is.null(m)) 
                        {
                        message("getting cached data")
                        return(m) ## Returns cached value
                        }
                ## if not in cache calculate the inverse
                data <- x$get()
                m <- solve(data)
                ## and store the value into cache               
                x$setmatrix(m)
                m
        }