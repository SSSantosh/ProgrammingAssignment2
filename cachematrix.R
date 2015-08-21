## This file contains two functions, to enable "caching", ie. to be able to
## avoid computing the time-consuming inverse operation for a matrix
## repeatedly and also to be able to store a once-computed inverse and retrieve
## it each time the same matrix is input. A new inverse is stored into the
## cache when the input matrix changes, by calling the first function with the
## new input matrix as its argument.


## This function takes as the argument, the matrix whose inverse is to be found
## and returns a list containing functions to 
## 1. Set the matrix.
## 2. Get the matrix.
## 3. Set the inverse of the matrix.
## 4. Get the inverse of the matrix. 
 
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  
  ## This is first function of the list. By using the "superassignment"
  ## operator "<<-", any matrix is assigned to the matrix object and a NULL is
  ## assigned to the inverse matrix object, in the global environment rather
  ## than the functional environment.
  
  set <- function(y){
    x <<- y
    inverse <<- NULL
  }
  
  
  ## This second function of the list returns the matrix object present in the
  ## global environment if the set function above is called beforehand. Else it
  ## returns the matrix object supplied as input argument.
  
  get <- function() x
  
  
  ## This is third function of the list. By using the "superassignment" operator
  ## "<<-", an inverse matrix is assigned to an object in the global environment.
  
  setInverse <- function(Minverse) inverse <<- Minverse
  
  ## This fourth function returns the object present in the global environment if
  ## the setInverse function above is called beforehand.Else it returns a NULL.
  
  getInverse <- function() inverse
  
  
  ## Return list of functions with names, set, get, setInverse, getInverse.
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}




## This function gives the inverse of a matrix, by solving the first time, 
## and by retrieving from cache subsequently. The input argument is the list
## object returned by the makeCacheMatrix function.

cacheSolve <- function(x, ...) {
  
  inverse <- x$getInverse()
  
  ## The first time, the above step would have assigned NULL to "inverse" and the 
  ## block is skipped. Subsequently, however, the inverse matrix located in the
  ## global environment is returned after printing a message indicating the above.   
  
  if (!is.null(inverse)){
        message("getting cached matrix")
        return(inverse) 
  }
  
  ## If it is the first time, the matrix is taken and inverse is computed by
  ## solve() function.   
  
  mat <- x$get()
  inverse <- solve(mat,...)
  
  ## And this inverse is stored in the global environment to be returned from    
  ## there subsequently. 
  
  x$setInverse(inverse)
  
  ## Return the computed inverse if it is the first time.
  inverse
}
