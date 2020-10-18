makeCacheMatrix <- function(x = matrix()) {
    i <- NULL #- initialization of two empty objects, x and i
    set <- function(y) {  #Assigns an input argument to object x in the parent environment
        x <<- y
        i <<- NULL    #assigns NULL to object i in the parent environment. This line of code clears out any i value that was cached by a previous execution of function
    }
    get <- function() x  #$assign a new variable with function(x)
    setinverse <- function(inverse) i <<- inverse #Since the x is not defined within get (), R retrieves it from the parent environment makeCacheMatrix ()
												  #use the form << - assignment operator to assign an input argument to the value i in the parent environment.
    getinverse <- function() i #get the value of i where is called
    list(set = set,  #you need this to run the function within the list
	                 #each element in the list is created with the syntax elementName = value
					 # naming of the list items is what allows  to use the $ extract operator form to access functions by name
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

cacheSolve <- function(x, ...) { 
	#This function computes the inverse of the  matrix returned by makeCacheMatrix 
	#cacheSolve starts with one argument, x and an ellipsis, which allows the caller to pass additional arguments to the function.
    i <- x$getinverse() #the function tries to extract the inverse matrix from the object passed as an argument, it calls the getinverse () function on the input object.
    if (!is.null(i)) {   #It then checks whether this value is NULL
        message("getting cached data")
        return(i) #If the value here is not NULL, we have a valid cached value and can return it to the parent environment
    }
    data <- x$get()
	#If the result of! Is.null (i) is FALSE, then cacheSolve() gets a vector from the input object, computes solve(), uses the setinverse() function on the input object to set the #mean in the input object, 
    i <- solve(data, ...)
    x$setinverse(i)  
    i  #and then returns the  value in parent environment by print matrix
}
