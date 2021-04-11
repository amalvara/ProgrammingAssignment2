## Put comments here that give an overall description of what your
## functions do


## La función makeCacheMatrix crea un objeto "matrix" especial que puede
## almacenar en caché su inversa.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- matrix
  getinverse <- function() m
  list(set = set, get = get, 
       setinverse = setinverse,
       getinverse = getinverse)
}

## Esta función calcula la inversa de la "matrix" especial devuelta 
## por makeCacheMatrix. Si la inversa ya se ha calculado (y la 
## matriz no ha cambiado), entonces cacheSolve debería recuperar la 
## inversa de la caché.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)){
          message("getting cache data")
          return(m)
        }
        data <- x$get()
        m < solve(data,...)
        x$setinverse(m)
        m
}
