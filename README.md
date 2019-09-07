# R-File
#For Assignment


makecachematrix <- function(x = matrix()) {
	  inversematrix <- NULL
	
	  setmatrix <- function(y) {
	    x <<- y
	    inversematrix <<- NULL
	  }
	
	  getmatrix <- function() x                              
	  setinverse <- function(inverse) inversematrix <<- inverse  
	  getinverse <- function() inversematrix                     
	  list(setmatrix = setmatrix, getmatrix = getmatrix,
	       setinverse = setinverse, getinverse = getinverse)
	
	}
	
	
	## Write a short comment describing this function
	
	cachesolve <- function(x, ...) {
	
	          inversematrix <- x$getinverse()
	        if(!is.null(inversematrix)) {                       
	          message("Getting Cached Invertible Matrix")   
	          return(inversematrix)                             
	        }
	
  
	        Matrixdata <- x$getmatrix()                     
	        inversematrix <- solve(Matrixdata, ...)             
	        x$setinverse(inversematrix)                          
	        return(inversematrix)                               
	        
	}
	