## Put comments here that give an overall description of what your
## functions do
## Write a short comment describing this function


makeCacheMatrix <- function(mx = matrix()) {
  inv <- NULL
  set <- function(y) {
    #message("entered makeVector/set funcion(). value of y is: ")
    #print(y)
           mx <<- y
          inv <<- NULL
          }
  get <- function() mx
  #message("Inside makeVector/get func now: printing mx")
  #print(mx)
   setmatrix  <- function(solve) inv <<- solve
       #message("Inside makeVector/setmean func now: assigning inv")
       #print(inv)
   getmatrix  <- function() inv
    list(set=set, get=get,
     setmatrix=setmatrix,
     getmatrix=getmatrix)
}
## Write a short comment describing this function
cacheSolve <- function(mx=matrix(), ...) {
  ## Return a matrix that is the inverse of 'mx'
  inv <- mx$getmatrix()
   if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  matrix <- mx$get()
    inv <- solve(matrix, ...)
  mx$setmatrix(inv)
  inv
}