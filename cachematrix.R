## Put comments here that give an overall description of what your
## functions do

## We want to create a special object (a list) that is able to 
## store a matrix and its inverse. The object is created using
## makeCacheMatrix function and the matrix inverse is calculated
## using cacheSolve function.



## Write a short comment describing this function
## makeCacheMatrix creates an object that stores a matrix and
## its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) m <<- inverse
  getinv <- function() m
  list(set = set, get = get,
       setinverse = setinv,
       getinverse = getinv)
}


## Write a short comment describing this function
## cacheSolve computes the inverse of matrix stored using
## makeCacheMatrix and stores the result in the same object

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}


###TESTING###
cs <- makeCacheMatrix()
cs$set(matrix(1:4,2,2))
cacheSolve(cs)
cs$getinverse()
#Output:
#     [,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5

solve(matrix(1:4,2,2))
#Output:
#     [,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
