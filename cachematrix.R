## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##the makeCacheMatrix builds a set of functions and return the 
##functions within a list to the parent Environment.
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL ##initializing x as function input and i as null
  set <- function(y) {
    x <<- y
    i <<- NULL
    ##Assign the input argument to the x object in the parent environment, and
    ##Assign the value of NULL to the i object in the parent environment. 
    ##This line of code clears any value of i that had been cached by a prior 
    ##execution of cacheSolve().
  }
  get <- function() x
  ##Since the symbol x is not defined within get(), 
  ##R retrieves it from the parent environment of makeCacheMatrix().
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  ##this creates a new object by returning a list
  ##When the function ends, it returns a fully formed object of 
  ##type makeCachematrix() to be used by downstream R code
}

## Write a short comment describing this function 
##cacheSolve() is required to populate and/or retrieve 
##the inverse from an object of type makeCacheMatrix()
cacheSolve <- function(x, ...) {
  ##starts with a single argument, x, and an ellipsis that allows the caller to 
  ##pass additional arguments into the function.
  ##Next, the function attempts to retrieve inverse from the object passed in 
  ##as the argument. First, it calls the getinverse() function on 
  ##the input object.
  i <- x$getinverse()
  ##Then it checks to see whether the result is NULL.
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  ##If the result of !is.null(i) is FALSE, cacheSolve() gets the matrix from the
  ##input object, calculates a solve(), uses the setinverse() function on the 
  ##input object to set the inverse in the input object, and then returns the value 
  ##of the inverse to the parent environment by printing the inverse object
  data <- x$get()
  i <- solve(data, ...)##calculating inverse and assigning to i
  
  x$setinverse(i)
  i
}
