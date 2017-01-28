#Matrix inversion is usually a costly computation and there may 
#be some benefit to caching the inverse of a matrix rather than 
#compute it repeatedly (there are also alternatives to matrix 
#inversion that we will not discuss here). Your assignment is to 
#write a pair of functions that cache the inverse of a matrix.


#This function creates a special "matrix" object, which is really
#a list containing a function to:

#1. set the matrix
#2. get the matrix
#3. set the inverse of the matrix
#4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, 
       get = get, 
       setinv = setinv, 
       getinv = getinv)
}


#The following function calculates the inverse of the special "matrix" above.
#It first checks to see if the inverse of the matrix has already been calculated. 
#If so, it gets the inverse from the cache and skips the computation. Otherwise, 
#it calculates the inverse of the data and sets the value of the inverse in the cache 
#via the setinv function. 

cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}

#Testing:
# > x <- matrix(1:4, nrow = 2, ncol = 2)
# > v <- makeCacheMatrix(x)
# > v$get()
#       [,1] [,2]
# [1,]    1    3
# [2,]    2    4

#Inverse of the matrix is NULL initially
# > v$getinv()
# NULL

#Inverse of the matrix was not cached in 1st run
#> cacheSolve(v)
#       [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5

#Inverse of the matrix was cached in 2nd run
# > cacheSolve(v)
# getting cached data
#       [,1] [,2]
# [1,]    1    3
# [2,]    2    4