## Catching the inverse of a Matrix, This was hard!

##  2 functions written
##       makeCacheMatrix: creates a special "matrix" object that can cache its inverse.
##       cacheSolve:      computes the inverse of the special "matrix" returned by makeCacheMatri. 
##                        If the inverse has already been calculated (and the matrix has not changed), 
##                        then the cachesolve retrieves the inverse from the cache.

makeCacheMatrix <- function(MAT = matrix()) {
      ## MAT: a square invertible matrix
      ## function returns list containing functions to set and get the matrix as well as set and get the inverse
      ##         this list is used as the input to cacheSolve()
      
      inv = NULL
      set = function(y) {
            MAT <<- y
            inv <<- NULL
      }
      get = function() MAT
      setinv = function(inverse) inv <<- inverse 
      getinv = function() inv
      list(set=set, get=get, setinv=setinv, getinv=getinv)
}

cacheSolve <- function(lMat, ...) {
      ## @lMat is the output of makeCacheMatrix()
      ## return: inverse of the original matrix input to makeCacheMatrix()
      
      inv = lMat$getinv()
      
      if (!is.null(inv)){
            message("using cached data")
            return(inv)
      }
      
      # else calculates the inverse 
      mat.data = lMat$get()
      inv = solve(mat.data, ...)
      
      lMat$setinv(inv)
      
      return(inv)
}