## These two functions allow to quickly obtain the inverse of a matrix. 
## This is possible because the inverse value is stored and recalled when needed,  without being calculated every time. 
## To do this we use the Lexical Scoping that allow R to find an object in the different enviroments.
## In this functions we are going to use the <<- operetor insite a fuction like setsolve. 
## This is necessary because we want to associate a value to "inv" in the parent enviroment (where it is defined) and not in the function's enviroment.

##The first function creates the matrix (x), and stores its inverse (named "inv" in the function) 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) inv <<- solve
  getsolve <- function() inv
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

##This second function analyzes the inverse of the matrix obtained previously (inv) 
##If the inverse has already been calculated and memorized (the inv object is not empty), 
##it is able to recall it and show it: the function prints the stored value. 
#If "inv" is still empty (this is the first time you make the computation)
##the function reverses the matrix with the solve function ad store it in the inv object, that is printed at the end.

cacheSolve <- function(x, ...) {
  inv <- x$getsolve() 
  if(!is.null(inv)) {                
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setsolve(inv)
  inv
}

#Test
m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
myMatrix_object <- makeCacheMatrix(m1)
cacheSolve(myMatrix_object)
cacheSolve(myMatrix_object)
