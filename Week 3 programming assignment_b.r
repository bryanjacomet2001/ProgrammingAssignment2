makeCacheMatrix <- function(x = matrix(sample(1:100,9),3,3)) {
  d <- NULL
  set <- function(y) {
    x <<- y
    d <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

cacheSolve <- function(x, ...) {
  d <- x$getsolve()
  if(!is.null(d)) {
    message("getting inversed matrix")
    return(d)
  }
  data <- x$get()
  d<- solve(data, ...)
  x$setsolve(d)
  d
}
