## Here we have "objects" of type makeCacheMatrix that contain matrix and it's inverse.
## (As far, as I can understand it)
## To avoid re-computing of inverse, we develop special interface through cacheSolve() function
## that allows us quickly access previously calculated inverse matrix if it is the same.
## User input supposed to be correct(A is square and invertible) and user is not violating object's inner structure

makeCacheMatrix <- function(x = matrix()) {  ## "constructor"
  m<-NULL ## initialize variable m, where reverse matrix is stored
  set <- function(y) { ## function, that overwrites matrix in current makeCacheMatrix object.
    x <<- y    
    m <<- NULL  ## we do not update inverse automatically
    ## m <<- solve(x)
  }
  get <- function() x  ## retrieve stored matrix
  setinverse <- function(y,...) {m <<- solve(y,...)} ## I guess, it's better to calculate here the inverse. 
  ##Thus makeCacheMatrix possess the function itself
  getinverse <- function() m ##retrieve value of m from makeCacheMatrix object ("cached data")
  list(set = set, get = get, ## interface:  "interface variable" = "variable from inner namespace"
       setinverse = setinverse,
       getinverse = getinverse)  
}

cacheSolve <- function(x, ...) { ##function, that works with makeCacheMatrix objects
  m <- x$getinverse() ##retrieve value of m, stored in x
  if(!is.null(m)) { ##if it exists, return it
    message("getting cached data")
    return(m) ##code breaks here
  }
  data <- x$get()  ##else get value of matrix, stored in x.  
  x$setinverse(data,...) ####calculate and store the inverse
  x$getinverse()  
}

## for output see Example_output.txt
