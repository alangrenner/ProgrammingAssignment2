## Put comments here that give an overall description of what your
## functions do
###Hello there.So here we have two functions. makeCacheMatrix will create the matrix 
###using the matrix function as X i.e:[ test<- makeCacheMatrix(matrix(1:4, nrow =2, ncol=2)) ]
###if you try to getinv at first, it will return NULL.
###cacheSolve function will find the inverse and return it.
###in case you reuse cacheSolve, it will display the message"getting the cached data now" and show the result.

## Write a short comment describing this function

makeCacheMatrix <- function (x = matrix()) {
  inv <- NULL         #this will initialize inverse as NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function(){x}        #this gets the matrix x
  setinv <- function(inverse) {inv <<- inverse}
  getinv <- function(){inv}   #this will get the inverse of the matrix
  list(set = set, 
       get = get, 
       setinv = setinv, 
       getinv = getinv)
}

## Write a short comment describing this function

cacheSolve <- function (x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)){      #this checks if inverse cache is null
    message ("getting the cached data now")
    return(inv)         #returns the value when not null
  }
  mat <- x$get()
  inv <- solve(mat, ...)    #calculates the inverse value
  x$setinv(inv)
  inv           #returns the inverse
}


