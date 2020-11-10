#R Programming example 'cachemean', week 2 on coursera.org - John Hopkins University
#Its function is to take vector in 'makeVector', cache it in memory, and create mean based on its value
#If mean already made from previous input, 'cachemean' will take it from memory instead of recalculating the mean

makeVector <- function(x = numeric()) {
  m <- NULL
  #parent function to get the value input
  
  set <- function(y) {
    x <<- y #Super assignment, <<-, never creates a variable in the current environment, but instead modifies an existing variable found in a parent environment. (https://adv-r.hadley.nz/environments.html : 7.2.4)
    m <<- NULL
  }
  #child function to set the value, does the same with parent
  
  get <- function() {
    x
  }
  #get input value
  
  setmean <- function(mean) {
    m <<- mean
  }
  #set mean value, will be inputted by 'cachemean' function
  
  getmean <- function() {
    m
  }
  #get mean value, if data still available from previous input, this will be used instead of calculating new value
  
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
  #create list, where value in 'set' stored in variable named 'set'
  
}

#first, input any vector into 'makeVector' function and assign into any variable (test <- makeVector(c(1,2,3,4,5)))
#function will create new variable to access (set, get, setmean, and getmean)
#test$get() result in printing the input
#test$set() error, this child function function is the same with one in parent
#test$setmean() error, this child function is to set the mean and used in 'cachemean'
#test$getmean() NULL, since at the beginning before cachemean runs, nothing stored in 'm'

cachemean <- function(x, ...) {
  #Input variable with makeVector as example, use cachemean(test), so test will be treated as 'x'
  
  m <- x$getmean()
  #this can be read as m <- test$getmean()
  
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}