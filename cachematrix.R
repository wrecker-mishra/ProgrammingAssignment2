## This function will take matrix as an argument and compute inverse of it.
## If inverse is already calculated and cached, it will not re-calculate it.

## To take advantage of lexical scoping, use by calling second function, passing## first function as argument.

## Usage : 1) source ("cachematrix.R") 2) cacheSolve(makeCacheMatrix(5*diag(3)))

## Function takes argument as matrix, and returns a vector with 
## 1.set value of matrix
## 2.get value of matrix
## 3.set value of inverse
## 4.get value of inverse

makeCacheMatrix <- function(x = matrix()) {
   m <- NULL
   set <- function(y) {
      x <<- y
      m <<- NULL
   }
   get <- function() x
   setinverse <- function(inverse) m <<- inverse
   getinverse <- function() m
   list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function uses the above matrix function call to generate inverse of matrix.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if (!is.null(m))  {
           message("getting cached data")
           return(m)
        }
        data <- x$get()
        #Function that calculates inverse of matrix
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
