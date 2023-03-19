## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(..., defaultData = matrix(), calc.funct = function(theData, ...) solve(theData)) {
  if(length(list(...)) > 0){
    myData <- list(...)[[1]]
  } else {
    myData <- defaultData
  }
  calcValue <- NULL
  reset <- function(newData) {
    myData <<- newData
    calcValue <<- NULL
  }
  calc.value <- function(...)calcValue <<- calc.funct(myData, ...)
  check.value <- function() calcValue
  list(reset = reset,
       check.value = check.value,
       calc.value = calc.value)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  
  calcValue <- x$check.value()
  if(!is.null(calcValue)) {
    message("getting cached data")
    return(calcValue)
  }
  calcValue <- x$calc.value(...)
  calcValue
        ## Return a matrix that is the inverse of 'x'
}
fred <- matrix(c(1, 4 , 9, 5), ncol = 2)
joe <- makeCacheMatrix()
cacheSolve(joe)
cacheSolve(joe)
