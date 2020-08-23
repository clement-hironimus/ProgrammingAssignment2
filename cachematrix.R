
#Function 1: A constructor function to create a new matrix cache
makeCacheMatrix <- function(x = matrix()) {
        
        result <- NULL #This where the cache is stored
        
        #Closures Functions
        
        #1. To set a new matrix value
        set <- function(newMatrix) {
                x <<- newMatrix #Change current matrix value
                result <<- NULL #Reset result to NULL
        }
        
        #2. To get the matrix (if cache not set yet)
        get <- function() {x}
        
        #3. To set the inverse value
        setInverse <- function(inverse) {result <<- inverse}
        getInverse <- function() {result} #To get the inverse value
        
        #4. Return a list of functions available
        return(list(
                set = set,
                get = get,
                setInverse = setInverse,
                getInverse = getInverse))
}

##Function 2: To return the inverse of a matrix
cacheSolve <- function(x, ...) {
        
        #To check if cache is stored already or not
        result <- x$getInverse()
        
        #If cache is found, return the result
        if(!is.null(result)) {
                message("getting cached data")
                return(result)
        }
        
        #Otherwise, compute the inverse and store the cache
        else {
                data <- x$get() #To get the matrix data
                result <- solve(data, ...) #Compute the matrix inverse
                x$setInverse(result) #Save the result to cache
                return(result) #Return the result
        }
}
