## Put comments here that give an overall description of what your
## functions do

#Reading Ref :https://github.com/lgreski/datasciencectacontent/blob/master/markdown/rprog-breakingDownMakeVector.md


# makeCacheMatrix takes matrix as input and gives list output
#This list output has four functions
#the output of makeCacheMatrix is input for cacheSolve which returns inverse of matrix
#which was input for makeCacheMatrix
#cacheSolve returns cached value if the inverse of matrix has been previously calculeted


## Write a short comment describing this function

#makeCacheMatrix takes matrix input and returns a list having four functions
# These are set, get, setsolve and getsolve
# The set function assigns input argument which is a matrix to x in parent environment and clears m
# The get function gets x from parent environment due to lexical scoping
# The getsolve function is getter for m
#The setsolve function assigns m in parent environment

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)

}


## Write a short comment describing this function
#cacheSolve takes the output of makeCacheMatrix as input
#Different functions of makeCachematrix are called through $
#It returns inverse of matrix input given to makeCacheMatrix
#If already that matrix has been run, it returns cahced value, otherwise calculates through solve function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
