## makeCacheMatrix is a function that creates a list possessing 
## the stores input argument [mx] (presumably a matrix) 
## the inverse of that matrix as matrix [inv]
## and defines getmatrix as a function to retrieve the inverse
## setmatrix as a function to create the inverse

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
## cacheSolve is a function that returns the inverse of the matrix,
## which is the value inv, using the functions getmatrix and setmatrix
## created above. First checks to see if inv already exists before
## calculating the inverse. Returns
## the non-NULL (assuming the input argument was an invertible matrix)
## inv, regardless.
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