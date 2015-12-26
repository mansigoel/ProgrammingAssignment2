## Put comments here that give an overall description of what your
## functions do
## If we compute the inverse of matrix everytime then it would proved to be costly. Hence to reduce multiple Matrix inversions,
## we can compute the inverse and cache it for further use. Though if there is a change in matrix, new inverse would be computed.

## Write a short comment describing this function
## This function creates a list containing a function to set and get the value of matrix
## as well as set and get the value of its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}




## Write a short comment describing this function
## This function checks first whether the inverse of matrix has been computed or not.
## If it's aready been stored in cache then printing the message it will return the inverse there and stop the further computation.
## Else it will solve for the inverse and then storing it in cache, it will return the newly computed inverse.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i
}


## Test Case

## x = rbind(c(1, 3), c(2, 4))
## newm = makeCacheMatrix(x)

## x 
##      [,1] [,2]
## [1,]    1    3
## [2,]    2    4

## cacheSolve(newm)
##           [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667

## cacheSolve(newm)
## getting cached data
##           [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667