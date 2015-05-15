## Matrix inversion optimisation
## Avoid calling solve() is inverse has already been computed

## makeCacheMatrix take a matrix x as argument and return a list of 4 functions
## set that store x in the creation environement of the functions
## get that return x
## setinverse that strore an object in the creation environement of the functions
## getinverse that return that object


makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
}


## Take as argument a "special matrix" x created with the makeCacheMatrix function
## and return the inverse of that matrix
## The first time cacheSolve is called for a "special matrix" x, inverse is 
## computed using the solve() function, and strored in the "creation environement" of x
## If cacheSolve is called again for the same "special matrix" x, the inverse,
## strored in the "creation environement" of x (or "Cache") is returned without 
## further call to solve()

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
        ## inv available in the "cache"
                return(inv)
        }
        m <- x$get()
        inv <- solve(m)
        x$setinverse(inv)
        inv
}
