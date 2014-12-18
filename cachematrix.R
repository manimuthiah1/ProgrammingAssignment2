#This program explain the "lexical scoping" strategy in r
# The lexical scoping makes r prog look for a value in different environment with a seq search.

## makeCacheMatrix function will define the special matrix which can be in global or any environment. 
# ex: a<-makeCacheMatrix(matrix(c(1,5,3,4),nrow=2,ncol=2)) will create spl matrix a


makeCacheMatrix <- function(x = matrix()) {
m <- NULL
  set <- function(y)
  {
    x <<- y
    m <<- NULL
    # initialize getinverse() as NULL
  }
  get <- function() x
  # a$get() will print the special matrix a
  setinverse <- function(inverse) m <<- inverse
  # setinverse will make the inverse matrix a forced value given by you.
  # Careful of using this as it will set the whatever you specify and
  # lexical scoping give this as value instead solve inverse
  getinverse <- function() m
  # a$getinverse() will provide the initial value of mean m which was set as "NULL'
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function is used to calculate the inverse of the special matrix created in makeCacheMatrix fn.
# r program have solve() function to calculate matrix inverse and used here.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  #The data now getting the special matrix created 
  m <- solve(data,...)
  x$setinverse(m)
  m
  # now the m value will get the inverse elements of the spl matrix
}
