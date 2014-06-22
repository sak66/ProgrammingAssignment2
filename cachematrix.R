

#' @title Make Cache Matrix, SAK66 21/06/16, for R Programming Assignment 2
#'
#' @description
#' \code{makeCacheMatrix} returns a list that contains the passed matrix and functions that can be used to set/get a
#' matrix ans set/get an inverse value (the cache). 
#' 
#' @details
#' This function constrcts a list object contaoining a matrix and functions to set/get the matrix and the inverse cache.
#' 
#' #' Below is a n example for using this function:
#' 
#' matrix = rbind(c(1, -1/4), c(-1/4, 1))
#' cm <- makeCacheMatrix(matrix)
#' 
#' cm$set(matrix)
#' matrix <- cm$get()
#' cm$setinverse(inverse)
#' inverse <- cm$getinverse()
#' 
#' 

makeCacheMatrix <- function(x = matrix()) {
                i <- NULL
                set <- function(y) {
                        x <<- y
                        i <<- NULL
                }
                get <- function() x
                setinverse <- function(inverse) i <<- inverse
                getinverse <- function() i
                list(set = set, get = get,
                     setinverse = setinverse,
                     getinverse = getinverse)
}


## Write a short comment describing this function

#' @title cache Solve, SAK66 21/06/16, for R Programming Assignment 2
#'
#' @description
#' \code{mcacheSolve} returns an inverse of a passed in cachedMatrix 
#' 
#' @details
#' This function return the inverse of a matrix contained in the cached matrix by either returning the 
#' previously calculated cached value, if it's available, or by using the solve function to calculte
#' the matrix's inverse if a cached value is not available. A calculated value is stored in the cached matrix
#' for use next time this function is called.
#' 
#' Below is a n example for using this function together with the makeCacheMatrix function.
#' 
#' matrix = rbind(c(1, -1/4), c(-1/4, 1))
#' cm <- makeCacheMatrix(matrix)
#' 
#' inverse <- cacheSolve(cm)
#' 
cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      i <- x$getinverse()
      if (!is.null(i)) {
              message("getting cached data")
              return(i)
      }
      i <- solve(x$get(), ...)
      x$setinverse(i)
      return(i)
}









