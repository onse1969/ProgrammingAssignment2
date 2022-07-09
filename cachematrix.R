## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# library(Matrix) ....https://cran.r-project.org/web/packages/Matrix/Matrix.pdf
# library(Matrix) ....https://cran.r-project.org/web/packages/Matrix/Matrix.pdf
library(MASS)   #...neded for ginv ...

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL 
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  get<-function()x             #function to get matrix x
  setinv<-function(inverse)inv<<-inverse
  getinv<-function(){ 
    inver<-ginv(x) # ....requires MASS library...
    # ....https://stackoverflow.com/questions/9122264/generalized-inverse-in-r
    # ....https://www.rdocumentation.org/packages/MASS/versions/7.3-57/topics/ginv 
    inver%*%x           #function to obtain inverse of the matrix
  }
  list(set = set, get = get, 
       setinv = setinv, 
       getinv = getinv)
}



## Write a short comment describing this function
#....the cachseolve function is used for / to get the cache data

cacheSolve <- function(x, ...) {
    inv<-x$getinv()                  
    if(!is.null(inv)){                 #.....is inverse is NUll ?
      message("getting cached data!")
      return(inv)                       #returns inverse value
    }
    data<-x$get()
    inv<-solve(data,...)              #calculates inverse value
    x$setinv(inv)
    inv   ## Returns inverse matrix
  }
